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
import Data.Maybe (mapMaybe)
import Data.Serialize.Put
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (decodeUtf8, encodeUtf8)
import Data.Word (Word8, Word32, Word64)
import System.Environment (getArgs)
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

interpret st@FS{..} is os es = do
  p <- S.parseFromStream packet is
  case p of
    FxpRealPath i "." Nothing -> do
      send os $ FxpName i [dirFxpName (Left fsHomeDirectory)] Nothing
      interpret st is os es

    FxpOpen i filename _ _ | filename `elem` fileNames fsHomeDirectory -> do
      send os $ FxpHandle i "0"
      interpret st is os es

    FxpOpen i _ _ _ -> do
      send os $ FxpStatus i fxNoSuchFile "No such file" "" ""
      interpret st is os es

    FxpRead i "0" offset len -> do
      send os $ FxpData i "hello" -- TODO File content
      interpret st is os es

    FxpWrite i "0" 0 bs -> do
      -- TODO Actually "write" bs.
      send os $ FxpStatus i fxOk "Success" "" ""
      interpret st is os es

    FxpOpenDir i dirname -> do
      send os $ FxpHandle i "0"
      interpret st is os es

    FxpReadDir i "0" | fsReadingDir == Nothing -> do
      send os $ FxpName i
        ([ (".", defaultAttrs)
         , ("..", defaultAttrs)
         ] ++
         map dirFxpName (dirEntries fsHomeDirectory)
        ) Nothing
      interpret st { fsReadingDir = Just "0" } is os es

    FxpReadDir i "0" -> do
      send os $ FxpStatus i fxEof "End of file" "" ""
      interpret st { fsReadingDir = Nothing } is os es

    FxpClose i "0" -> do
      send os $ FxpStatus i fxOk "Success" "" ""
      interpret st is os es

    FxpLStat i "." -> do
      send os $ FxpAttrs i (dirAttrs fsHomeDirectory)
      interpret st is os es

    FxpLStat i name | Just File{..} <- getFile name fsHomeDirectory -> do
      send os $ FxpAttrs i fileAttrs
      interpret st is os es

    FxpLStat i _ -> do
      send os $ FxpStatus i fxNoSuchFile "No such file" "" ""
      interpret st is os es

    -- TODO Probably answer with FxpStatus.
    _ -> do
      let Just i = packetId p
      send os $ FxpStatus i fxOpUnsupported "Unsupported message" "" ""
      interpret st is os es

data FS = FS
  { fsHomeDirectory :: Directory
  , fsReadingDir :: Maybe ByteString
  -- ^ Keep track of an existing FxpReadDir.
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

dirFxpName (Left Directory{..}) = (dirName, dirAttrs)
dirFxpName (Right File{..}) = (fileName, fileAttrs)

fileNames = mapMaybe f . dirEntries
  where f (Right File{..}) = Just fileName
        f _ = Nothing

getFile name Directory{..} = case filter f dirEntries of
  [Right x] -> Just x
  _ -> Nothing
  where f (Right File{..}) = fileName == name
        f _ = False

initialState = FS
  { fsHomeDirectory = Directory "sftp"
    someAttrs
    [ Left someDir
    , Right (File "hello.txt"
        (Attrs (Just . fromIntegral $ BC.length "hello") (Just (1000, 1000)) (Just 33188)
         (Just (1441313037, 1441313037)) [])
        "hello")
    ]
  , fsReadingDir = Nothing
  }

someAttrs =
  Attrs (Just 4096) (Just (1000, 1000)) (Just 16893)
   (Just (1441313037, 1441313037)) []

someDir = Directory "somedir"
  someAttrs
  []

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

-- putAttrs :: Parser Attrs
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
