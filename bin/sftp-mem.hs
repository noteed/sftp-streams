{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Simple program acting as a static read-only SFTP server.
module Main (main) where

import Control.Applicative ((<$>), (<*), (<*>))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, MVar)
import Control.Exception (finally)
import Control.Monad (when)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import qualified Data.Attoparsec.ByteString as AB (take)
import Data.Attoparsec.ByteString hiding (parse, take)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BC
import Data.List (partition)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Serialize.Put
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (decodeUtf8, encodeUtf8)
import Data.Word (Word8, Word32, Word64)
import System.Environment (getArgs)
import System.FilePath (joinPath, splitDirectories)
import System.IO (hClose, hFlush, hPutStrLn, hSetBuffering, stdin, stderr, stdout, BufferMode(..))
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as S
import qualified System.IO.Streams.Attoparsec as S
import qualified System.Process as P

import Network.SFTP.Messages

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  args <- getArgs

  start initialState S.stdin S.stdout S.stderr

start st is os es = do
  p <- S.parseFromStream packet is
  case p of
    FxpInit 3 -> do
      send os $ FxpVersion 3 []
      interpret st is os es
    -- TODO Probably answer with FxpStatus.
    _ -> error "Unsupported first message"

interpret st@ST{..} is os es = do
  p <- S.parseFromStream packet is
  case p of
    FxpRealPath i "." Nothing -> do
      let cdir = currentDir st
      send os $ FxpName i [entryFxpName (Left cdir)] Nothing
      interpret st is os es

    FxpRealPath i path Nothing -> do
      case splitPath path of
        "/":rest -> case lookupDir' rest stRootDirectory of
          Just dir -> do
            send os $ FxpName i [entryFxpName' ("/":rest) (Left dir)] Nothing
            interpret st {stCurrentDirectory = rest} is os es
          Nothing -> do
            send os $ FxpStatus i fxNoSuchFile "No such file" "" ""
            interpret st is os es
        _ -> do
          send os $ FxpStatus i fxOpUnsupported "Unsupported relative path" "" ""
          interpret st is os es

    FxpOpen i path _ _ ->
      case splitPath path of
        "/":rest | length rest > 0 -> case lookupEntry' stRootDirectory rest of
          Just (Right _) -> do
            let (st', h) = newHandle ("/":rest) st
            send os $ FxpHandle i h
            interpret st' is os es
          Nothing -> do
            send os $ FxpStatus i fxNoSuchFile "No such file" "" ""
            interpret st is os es
        _ -> do
          send os $ FxpStatus i fxNoSuchFile "No such file" "" ""
          interpret st is os es

    FxpRead i h 0 len | len > 100 -> do -- TODO Bigger file
      case lookupHandle st h of
        Just (Right File{..}) -> do
          send os $ FxpData i fileContent
          interpret st is os es
        Nothing -> do
          send os $ FxpStatus i fxNoSuchFile "No such handle" "" "" -- TODO Correct error code and message.
          interpret st is os es

    FxpRead i h _ _ -> do
      send os $ FxpStatus i fxEof "End of file" "" ""
      interpret st is os es

    FxpWrite i h 0 bs -> do
      -- TODO Actually "write" bs.
      send os $ FxpStatus i fxOk "Success" "" ""
      interpret st is os es

    FxpOpenDir i path -> do
      let (st', h) = newHandle (splitPath path) st
      send os $ FxpHandle i h
      interpret st' is os es

    FxpReadDir i h | stReadingDir == Nothing -> do
      case lookupHandle st h of
        Just (Left d) -> do
          send os $ FxpName i
            ([ (".", defaultAttrs)
             , ("..", defaultAttrs)
             ] ++
             map entryFxpName (dirEntries d)
            ) Nothing
          interpret st { stReadingDir = Just h } is os es
        _ -> do
          send os $ FxpStatus i fxNoSuchFile "No such handle" "" "" -- TODO Correct error code and message.
          interpret st is os es

    FxpReadDir i h -> do
      send os $ FxpStatus i fxEof "End of file" "" ""
      interpret st { stReadingDir = Nothing } is os es

    FxpClose i h -> do
      let st' = removeHandle st h
      send os $ FxpStatus i fxOk "Success" "" ""
      interpret st' is os es

    FxpLStat i path ->
      interpretStat st i path is os es

    FxpStat i path -> do
      interpretStat st i path is os es

    -- TODO Probably answer with FxpStatus.
    _ -> do
      let Just i = packetId p
      send os $ FxpStatus i fxOpUnsupported "Unsupported message" "" ""
      interpret st is os es

interpretStat st i "." is os es = do
  send os $ FxpAttrs i (dirAttrs (currentDir st))
  interpret st is os es
interpretStat st@ST{..} i path is os es = do
  case splitPath path of
    ["/"] -> do
      send os $ FxpAttrs i (dirAttrs stRootDirectory)
      interpret st is os es
    "/":rest -> do
      case lookupEntry' stRootDirectory rest of
        Just (Right File{..}) -> do
          send os $ FxpAttrs i fileAttrs
          interpret st is os es
        Just (Left Directory{..}) -> do
          send os $ FxpAttrs i dirAttrs
          interpret st is os es
        Nothing -> do
          send os $ FxpStatus i fxNoSuchFile "No such file" "" ""
          interpret st is os es
    _ -> do
      send os $ FxpStatus i fxOpUnsupported "Unsupported relative path" "" ""
      interpret st is os es

data ST = ST
  { stRootDirectory :: Directory
  , stCurrentDirectory :: [Text]
  -- ^ List of directories. When empty, means root.
  , stReadingDir :: Maybe ByteString
  -- ^ Keep track of an existing FxpReadDir.
  , stHandles :: Map ByteString [Text]
  -- ^ Mapping from handles to open files.
  , stHandleNames :: [ByteString]
  -- ^ Supply of handle names.
  }

data Directory =
  Directory
  { dirName :: Text
  , dirAttrs :: Attrs
  , dirEntries :: [Either Directory File]
  }

data File =
  File
  { fileName :: Text
  , fileAttrs :: Attrs
  , fileContent :: ByteString
  }

newHandle path st@ST{..} =
  let h:rest = stHandleNames
      st' = st { stHandles = M.insert h path stHandles , stHandleNames = rest }
  in (st', h)

lookupHandle ST{..} h = case M.lookup h stHandles of
  Just ["/"] -> Just (Left stRootDirectory)
  Just ("/":rest) -> lookupEntry' stRootDirectory rest
  _ -> Nothing

removeHandle st@ST{..} h =
  st { stHandles = M.delete h stHandles , stHandleNames = h : stHandleNames }

entryFxpName (Left Directory{..}) = (dirName, dirAttrs)
entryFxpName (Right File{..}) = (fileName, fileAttrs)

entryFxpName' path (Left Directory{..}) =
  (joinPath' path, dirAttrs)
entryFxpName' path (Right File{..}) =
  (joinPath' path, fileAttrs)

splitPath path = go path'
  where
  path' = filter (/= ".")
    (map T.pack (splitDirectories (T.unpack path)))
  go [] = []
  go [x] = [x]
  go (x:"..":rest) = rest
  go (x:rest) = x : go rest

joinPath' texts = T.pack (joinPath (map T.unpack texts))

fileNames = mapMaybe f . dirEntries
  where f (Right File{..}) = Just fileName
        f _ = Nothing

lookupFile' dir [] = error "lookupFile' called with empty path"
lookupFile' dir path = case lookupEntry' dir path of
  Just (Right f) -> Just f
  _ -> Nothing

currentDir :: ST -> Directory
currentDir ST{..} = case lookupDir' stCurrentDirectory stRootDirectory of
  Just d -> d
  Nothing -> error "Mal-formed stCurrentDirectory."

lookupDir :: Text -> Directory -> Maybe Directory
lookupDir name Directory{..} = case filter f dirEntries of
  [Left x] -> Just x
  _ -> Nothing
  where f (Left Directory{..}) = dirName == name
        f _ = False

lookupDir' :: [Text] -> Directory -> Maybe Directory
lookupDir' path dir = go path dir
  where
  go [] d = Just d
  go (x:xs) d = case lookupDir x d of
    Nothing -> Nothing
    Just d' -> go xs d'

lookupEntry name Directory{..} = case filter f dirEntries of
  [Right x] -> Just (Right x)
  [Left x] -> Just (Left x)
  _ -> Nothing
  where f (Right File{..}) = fileName == name
        f (Left Directory{..}) = dirName == name
        f _ = False

lookupEntry' _ [] = error "lookupEntry' called with empty path"
lookupEntry' dir path = lookupDir' (init path) dir >>= lookupEntry (last path)

initialState = ST
  { stRootDirectory = Directory "/"
    someAttrs
    [ Left projectDir
    , Right helloFile
    ]
  , stCurrentDirectory = []
  , stReadingDir = Nothing
  , stHandles = M.empty
  -- Infinite supply of handle names.
  , stHandleNames = map (BC.pack . show) [0..]
  }

someAttrs =
  Attrs (Just 4096) (Just (1000, 1000)) (Just 16893)
   (Just (1441313037, 1441313037)) []

projectDir = Directory "project" someAttrs
  [ Left emptyDir
  , Left testsDir
  , Right readmeFile
  , Right helloFile
  ]

emptyDir = Directory "empty" someAttrs []

testsDir = Directory "tests" someAttrs
  [ Right testFile
  ]

readmeFile = file "README.md" "# Some project\n"

helloFile = file "hello.txt" "Hello world.\n"

testFile = file "test-00.txt" "Not a test.\n"

file name content = File name
  (Attrs (Just . fromIntegral $ BC.length content) (Just (1000, 1000)) (Just 33188)
  (Just (1441313037, 1441313037)) [])
  content

defaultAttrs = Attrs
  { attrsSize = Nothing
  , attrsUidGid = Nothing
  , attrsPermission = Nothing
  , attrsAccessTime = Nothing
  , attrsExtended = []
  }

send :: S.OutputStream ByteString -> Packet -> IO ()
send os p = S.write (Just . runPut $ serialize p) os

serialize p = case p of
  FxpVersion version [] -> do
    putWord32be 5 -- size
    putWord8 2 -- type
    putWord32be version

  -- Last field is not part of protocol 3.
  FxpName i filenames _ -> do
    let payload = runPut $ do
          putWord8 104 -- type
          putWord32be . fromIntegral $ unRequestId i
          putWord32be . fromIntegral $ length filenames
          mapM_ (\(s, a) -> do
            let s' = T.encodeUtf8 s
            putWord32be . fromIntegral $ BC.length s'
            putByteString s'
            putWord32be 0 -- longname, not in protocol 6
            putAttrs a
            ) filenames
    putWord32be . fromIntegral $ BC.length payload
    putByteString payload

  FxpHandle i h -> do
    putWord32be . fromIntegral $ 1 + 4 + 4 + BC.length h -- size
    putWord8 102 -- type
    putWord32be . fromIntegral $ unRequestId i
    putWord32be . fromIntegral $ BC.length h
    putByteString h

  FxpStatus i code msg tag bs -> do
    let msg' = T.encodeUtf8 msg
        tag' = T.encodeUtf8 tag
    putWord32be . fromIntegral $
      1 + 4 + 4 + 4 + BC.length msg' + 4 + BC.length tag' + BC.length bs -- size
    putWord8 101 -- type
    putWord32be . fromIntegral $ unRequestId i
    putWord32be code
    putWord32be . fromIntegral $ BC.length msg'
    putByteString msg'
    putWord32be . fromIntegral $ BC.length tag'
    putByteString tag'
    putByteString bs

  FxpAttrs i a -> do
    let payload = runPut $ do
          putWord8 105 -- type
          putWord32be . fromIntegral $ unRequestId i
          putAttrs a
    putWord32be . fromIntegral $ BC.length payload
    putByteString payload

  FxpData i bs -> do
    putWord32be . fromIntegral $
      1 + 4 + 4 + BC.length bs -- size
    putWord8 103 -- type
    putWord32be . fromIntegral $ unRequestId i
    putWord32be . fromIntegral $ BC.length bs
    putByteString bs

fxpData n = do
  i <- requestId
  bs <- str $ n - 4
  when (n - 4 - 4 - BC.length bs /= 0) $ fail "Invalid data size."
  return $ FxpData i bs

fxpStatus :: Int -> Parser Packet
fxpStatus n = do
  i <- requestId
  code <- word32BE
  msg <- str $ n - 8
  tag <- str $ n - 8 - 4 - BC.length msg
  bs <- AB.take $ n - 8 - 8 - BC.length msg - BC.length tag
  return $ FxpStatus i code (T.decodeUtf8 msg) (T.decodeUtf8 tag) bs

putAttrs :: Attrs -> PutM ()
putAttrs Attrs{..} = do
  let flags =
        maybe 0x00 (const flagSize) attrsSize .|.
        maybe 0x00 (const flagUidGid) attrsUidGid .|.
        maybe 0x00 (const flagPermissions) attrsPermission .|.
        maybe 0x00 (const flagAccessTime) attrsAccessTime .|.
        if null attrsExtended then 0x00 else flagExtended
  putWord32be flags
  maybe (return ()) putWord64be attrsSize
  maybe (return ()) (\(uid, gid) -> putWord32be uid >> putWord32be gid) attrsUidGid
  maybe (return ()) putWord32be attrsPermission
  maybe (return ()) (\(a, b) -> putWord32be a >> putWord32be b) attrsAccessTime
  mapM_ (error "attrsExtended") attrsExtended
