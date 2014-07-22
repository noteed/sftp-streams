{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Simple program to proxy a SFTP session. I.e. a SFTP client will spawn us
-- through SSH instead of a real sftp executable. It won't see any difference
-- as we are fully transparent, but we can se what the protocol looks like.
module Main (main) where

import Control.Applicative ((<$>), (<*), (<*>))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, MVar)
import Control.Exception (finally)
import Control.Monad (when)
import qualified Data.Attoparsec.ByteString as AB (take)
import Data.Attoparsec.ByteString hiding (take)
import Data.Bits (unsafeShiftL, (.&.), Bits)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (decodeUtf8)
import Data.Word (Word8, Word32, Word64)
import System.Environment (getArgs)
import System.IO (hClose, hFlush, hPutStrLn, hSetBuffering, stdin, stderr, stdout, BufferMode(..))
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as S
import qualified System.IO.Streams.Attoparsec as S
import qualified System.Process as P

debug :: String -> IO ()
debug s = hPutStrLn stderr ("### " ++ s) >> hFlush stderr

----------------------------------------------------------------------
-- Main loop:
-- We connect our stdin, stdout, and stderr to the real sftp process
-- and feed at the same time our parser.
----------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  args <- getArgs
  debug $ show args

  (sftpInp, sftpOut, sftpErr, _) <-
    runInteractiveProcess "/usr/lib/openssh/sftp-server-original"
      args Nothing Nothing

  (is, is') <- splitIS S.stdin
  (is2, is2') <- splitIS sftpOut

  inThread <- forkIO $ S.connect is' sftpInp
  errThread <- forkIO $ S.connect sftpErr S.stderr
  _ <- forkIO $ S.connect is2' S.stdout
  S.withFileAsOutput "/home/sftp/debug.txt" $ \observe -> do
    _ <- forkIO $ protocol observe ">>> " is
    protocol observe "<<< " is2
  -- takeMVar inThread
  -- takeMVar errThread

waitableForkIO :: IO () -> IO (MVar ())
waitableForkIO io = do
  mvar <- newEmptyMVar
  _ <- forkIO (io `finally` putMVar mvar ())
  return mvar

-- | Similar to S.runInteractiveProcess but set the different (wrapped)
-- Handles to NoBuffering.
runInteractiveProcess :: FilePath
  -> [String]
  -> Maybe FilePath
  -> Maybe [(String, String)]
  -> IO (OutputStream ByteString, InputStream ByteString,
    InputStream ByteString, P.ProcessHandle)
runInteractiveProcess cmd args wd env = do
    (hin, hout, herr, ph) <- P.runInteractiveProcess cmd args wd env
    hSetBuffering hin NoBuffering
    hSetBuffering hout NoBuffering
    hSetBuffering herr NoBuffering
    sIn  <- S.handleToOutputStream hin >>=
            S.atEndOfOutput (hClose hin) >>=
            S.lockingOutputStream
    sOut <- S.handleToInputStream hout >>=
            S.atEndOfInput (hClose hout) >>=
            S.lockingInputStream
    sErr <- S.handleToInputStream herr >>=
            S.atEndOfInput (hClose herr) >>=
            S.lockingInputStream
    return (sIn, sOut, sErr, ph)

splitIS :: InputStream a -> IO (InputStream a, InputStream a)
splitIS is = do
  mvar <- newEmptyMVar
  is0 <- S.makeInputStream (S.read is >>= \m -> putMVar mvar m >> return m)
  is1 <- S.makeInputStream (takeMVar mvar)
  return (is0, is1)

----------------------------------------------------------------------
-- Main io-streams parsers
----------------------------------------------------------------------

-- | `observe` is just an output stream where to show debugging info.
-- This is normally stderr. `prefix` is a few characters than can be
-- used to identify different source, typically the sftp client output
-- versus the sftp server output.
protocol :: OutputStream ByteString -> ByteString -> InputStream ByteString -> IO ()
protocol observe prefix is = do
  -- Actual order of packet must be:
  -- - init
  -- - extended / version-select (if any)
  -- ...
  packets <- S.parseFromStream (many' packet) is
  mapM_ (flip S.write observe . Just . (prefix `BC.append`) . BC.pack .
    (++ "\n") . display) packets
  return ()

display :: Packet -> String
display p = case p of
  FxpInit version -> "SSH_FXP_INIT " ++ show version
  FxpVersion version ps -> "SSH_FXP_VERSION " ++ show version ++ "\n  " ++ show ps
  FxpOpen i filename flags attrs ->
    "SSH_FXP_OPEN " ++ T.unpack filename ++ " " ++ take 64 (show attrs)
  FxpClose i h -> "SSH_FXP_CLOSE " ++ BC.unpack (Base16.encode h)
  FxpRead bs -> "SSH_FXP_READ " ++ take 64 (show bs)
  FxpWrite i h offset bs -> "SSH_FXP_WRITE " ++ BC.unpack (Base16.encode h) ++ " " ++ show offset ++ "\n" ++ take 64 (show bs)
  FxpLStat i path -> "SSH_FXP_LSTAT " ++ T.unpack path
  FxpFStat bs -> "SSH_FXP_FSTAT " ++ take 64 (show bs)
  FxpSetStat i path bs -> "SSH_FXP_SETSTAT " ++ T.unpack path
  FxpFSetStat i h bs -> "SSH_FXP_FSETSTAT " ++ BC.unpack (Base16.encode h)
  FxpOpenDir i dirname -> "SSH_FXP_OPENDIR " ++ T.unpack dirname
  FxpReadDir bs -> "SSH_FXP_READDIR " ++ take 64 (show bs)
  FxpRemove bs -> "SSH_FXP_REMOVE " ++ take 64 (show bs)
  FxpMkDir i dirname bs -> "SSH_FXP_MKDIR " ++ T.unpack dirname
  FxpRmDir bs -> "SSH_FXP_RMDIR " ++ take 64 (show bs)
  FxpRealPath i path m -> "SSH_FXP_REALPATH " ++ T.unpack path ++ " " ++ show m
  FxpStat i path -> "SSH_FXP_STAT " ++ T.unpack path
  FxpRename bs -> "SSH_FXP_RENAME " ++ take 64 (show bs)
  FxpReadLink bs -> "SSH_FXP_READLINK " ++ take 64 (show bs)

  FxpLink bs -> "SSH_FXP_LINK " ++ take 64 (show bs)
  FxpBlock bs -> "SSH_FXP_BLOCK " ++ take 64 (show bs)
  FxpUnblock bs -> "SSH_FXP_UNBLOCK " ++ take 64 (show bs)

  FxpStatus i code msg tag bs -> "SSH_FXP_STATUS " ++ show code ++ " " ++
    T.unpack msg ++ " " ++ take 64 (show bs)
  -- Handles are opaque strings; we choose to display them in hex.
  FxpHandle i h -> "SSH_FXP_HANDLE " ++ BC.unpack (Base16.encode h)
  FxpData bs -> "SSH_FXP_DATA " ++ take 64 (show bs)
  FxpName i filenames m -> "SSH_FXP_NAME " ++ show (map fst filenames) ++ show (map snd filenames)
  FxpAttrs i bs -> "SSH_FXP_ATTRS " ++ show bs

  FxpExtended bs -> "SSH_FXP_EXTENDED " ++ take 64 (show bs)
  FxpExtendedReply bs -> "SSH_FXP_EXTENDED_REPLY " ++ take 64 (show bs)
  Unknown bs -> take 64 $ show bs

----------------------------------------------------------------------
-- Main parsers
----------------------------------------------------------------------

packet :: Parser Packet
packet = do
  n <- pred . fromIntegral <$> word32BE
  t <- anyWord8
  case t of
    1 -> fxpInit n
    2 -> fxpVersion n
    3 -> fxpOpen n
    4 -> fxpClose n
    5 -> fxpRead n
    6 -> fxpWrite n
    7 -> fxpLStat n
    8 -> fxpFStat n
    9 -> fxpSetStat n
    10 -> fxpFSetStat n
    11 -> fxpOpenDir n
    12 -> fxpReadDir n
    13 -> fxpRemove n
    14 -> fxpMkDir n
    15 -> fxpRmDir n
    16 -> fxpRealPath n
    17 -> fxpStat n
    18 -> fxpRename n
    19 -> fxpReadLink n

    21 -> fxpLink n
    22 -> fxpBlock n
    23 -> fxpUnblock n

    101 -> fxpStatus n
    102 -> fxpHandle n
    103 -> fxpData n
    104 -> fxpName n
    105 -> fxpAttrs n

    200 -> fxpExtended n
    201 -> fxpExtendedReply n
    _ -> do
      bs <- AB.take n
      return $ Unknown bs

fxpInit :: Int -> Parser Packet
fxpInit n = do
  when (n /= 4) $ fail "Invalid SSH_FXP_INIT length."
  version <- word32BE
  return $ FxpInit version

fxpVersion :: Int -> Parser Packet
fxpVersion n = do
  when (n < 4) $ fail "Invalid SSH_FXP_VERSION length."
  version <- word32BE
  ps <- extensionPairs $ n - 4
  return $ FxpVersion version ps

fxpOpen :: Int -> Parser Packet
fxpOpen n = do
  i <- requestId
  filename <- str $ n - 4
  -- desiredAccess <- word32BE -- not in protocol 3
  flags <- word32BE
  bs <- AB.take $ n - 4 - 4 - BC.length filename - 4 -- - 4
  attrs <- either fail return $ parseOnly (readAttrs <* endOfInput) bs
  return $ FxpOpen i (T.decodeUtf8 filename) flags attrs

fxpClose :: Int -> Parser Packet
fxpClose n = do
  i <- requestId
  s <- str $ n - 4
  return $ FxpClose i s

fxpRead :: Int -> Parser Packet
fxpRead n = FxpRead <$> AB.take n

fxpWrite :: Int -> Parser Packet
fxpWrite n = do
  i <- requestId
  h <- str $ n - 4
  offset <- word64BE
  bs <- str $ n - 4 - 4 - BC.length h - 8
  when (n - 4 -4 - BC.length h - 8 - 4 - BC.length bs /= 0) $ fail "Invalid write size."
  return $ FxpWrite i h offset bs

fxpLStat :: Int -> Parser Packet
fxpLStat n = do
  i <- requestId
  path <- str $ n - 4
  return $ FxpLStat i (T.decodeUtf8 path)

fxpFStat :: Int -> Parser Packet
fxpFStat n = FxpFStat <$> AB.take n

fxpSetStat :: Int -> Parser Packet
fxpSetStat n = do
  i <- requestId
  path <- str $ n - 4
  bs <- AB.take $ n - 4 - 4 - BC.length path
  attrs <- either fail return $ parseOnly (readAttrs <* endOfInput) bs
  return $ FxpSetStat i (T.decodeUtf8 path) attrs

fxpFSetStat :: Int -> Parser Packet
fxpFSetStat n = do
  i <- requestId
  h <- str $ n - 4
  bs <- AB.take $ n - 4 - 4 - BC.length h
  attrs <- either fail return $ parseOnly (readAttrs <* endOfInput) bs
  return $ FxpFSetStat i h attrs

fxpOpenDir :: Int -> Parser Packet
fxpOpenDir n = do
  i <- requestId
  dirname <- str $ n - 4
  return $ FxpOpenDir i (T.decodeUtf8 dirname)

fxpReadDir :: Int -> Parser Packet
fxpReadDir n = FxpReadDir <$> AB.take n

fxpRemove :: Int -> Parser Packet
fxpRemove n = FxpRemove <$> AB.take n

fxpMkDir :: Int -> Parser Packet
fxpMkDir n = do
  i <- requestId
  dirname <- str $ n - 4
  bs <- AB.take $ n - 4 - 4 - BC.length dirname
  attrs <- either fail return $ parseOnly (readAttrs <* endOfInput) bs
  return $ FxpMkDir i (T.decodeUtf8 dirname) attrs

fxpRmDir :: Int -> Parser Packet
fxpRmDir n = FxpRmDir <$> AB.take n

fxpRealPath :: Int -> Parser Packet
fxpRealPath n = do
  i <- requestId
  path <- str $ n - 4
  let n' = n - 4 - 4 - BC.length path
  if (n' > 0)
    then do
      controlByte <- anyWord8
      composes <- strs $ n' - 1
      return $ FxpRealPath i (T.decodeUtf8 path) (Just (controlByte, map T.decodeUtf8 composes))
    else
      return $ FxpRealPath i (T.decodeUtf8 path) Nothing

fxpStat :: Int -> Parser Packet
fxpStat n = do
  i <- requestId
  path <- str $ n - 4
  return $ FxpStat i (T.decodeUtf8 path)

fxpRename :: Int -> Parser Packet
fxpRename n = FxpRename <$> AB.take n

fxpReadLink :: Int -> Parser Packet
fxpReadLink n = FxpReadLink <$> AB.take n
                 
fxpLink :: Int -> Parser Packet
fxpLink n = FxpLink <$> AB.take n

fxpBlock :: Int -> Parser Packet
fxpBlock n = FxpBlock <$> AB.take n

fxpUnblock :: Int -> Parser Packet
fxpUnblock n = FxpUnblock <$> AB.take n
                 
fxpStatus :: Int -> Parser Packet
fxpStatus n = do
  i <- requestId
  code <- word32BE
  msg <- str $ n - 8
  tag <- str $ n - 8 - 4 - BC.length msg
  bs <- AB.take $ n - 8 - 8 - BC.length msg - BC.length tag
  return $ FxpStatus i code (T.decodeUtf8 msg) (T.decodeUtf8 tag) bs

fxpHandle :: Int -> Parser Packet
fxpHandle n = do
  i <- requestId
  -- Must be a string no longer than 256 bytes.
  when (n - 8 > 256) $ fail "Invalid handle size."
  h <- str $ n - 4
  return $ FxpHandle i h

fxpData :: Int -> Parser Packet
fxpData n = FxpData <$> AB.take n

fxpName :: Int -> Parser Packet
fxpName n = do
  bs <- AB.take n
  either fail return $ flip parseOnly bs $ do
    i <- requestId
    c <- fromIntegral <$> word32BE
    filenames <- count c $ do
      s <- str'
      _ <- str' -- longname, in protocol 3, not 6.
      attrs <- readAttrs
      return $ (T.decodeUtf8 s, attrs)
    m <- option Nothing (Just . (/= 0) <$> anyWord8)
    endOfInput
    return $ FxpName i filenames m
--  return $ FxpName (RequestId 0) [] Nothing

fxpAttrs :: Int -> Parser Packet
fxpAttrs n = do
  bs <- AB.take n
  either fail return $ flip parseOnly bs $ do
    i <- requestId
    attrs <- readAttrs
    endOfInput
    return $ FxpAttrs i attrs
                  
fxpExtended :: Int -> Parser Packet
fxpExtended n = FxpExtended <$> AB.take n

fxpExtendedReply :: Int -> Parser Packet
fxpExtendedReply n = FxpExtendedReply <$> AB.take n

-- Protocol 3.
readAttrs :: Parser Attrs
readAttrs = do
  flags <- word32BE
  let on :: Word32 -> (Parser a) -> Parser (Maybe a)
      on flag f =
        if (flags `contains` flag)
        then Just <$> f
        else return Nothing
  msize <- on flagSize word64BE
  mids <- on flagUidGid $
    (,) <$> word32BE -- uid
        <*> word32BE -- gid
  mperm <- on flagPermissions word32BE
  matime <- on flagAccessTime $
    (,) <$> word32BE
        <*> word32BE
  exts <- if flags `contains` flagExtended
    then do
      c <- fromIntegral <$> word32BE
      count c extensionPair'
    else return []
  return Attrs
    { attrsSize = msize
    , attrsUidGid = mids
    , attrsPermission = mperm
    , attrsAccessTime = matime
    , attrsExtended = exts
    }

-- Protocol 6.
readAttrs6 :: Parser ByteString
readAttrs6 = do
  flags <- word32BE
  t <- anyWord8
  let on :: Word32 -> (Parser a) -> Parser ()
      on flag f = when (flags `contains` flag) $ f >> return ()
  on flagSize $ word64BE
  on flagAllocationSize $ word64BE
  on flagOwnerGroup $ str' -- owner
  on flagOwnerGroup $ str' -- group
  on flagPermissions $ word32BE
  on flagAccessTime $ do
    word64BE
    on flagSubSecondTimes $ word32BE
  on flagCreateTime $ do
    word64BE
    on flagSubSecondTimes $ word32BE
  on flagModifyTime $ do
    word64BE
    on flagSubSecondTimes $ word32BE
  on flagCTime $ do
    word64BE
    on flagSubSecondTimes $ word32BE
  on flagAcl $ str'
  on flagBits $ word32BE
  on flagBits $ word32BE
  on flagTextHint $ anyWord8
  on flagMimeType $ str'
  on flagLinkCount $ word32BE
  on flagUntranslatedName $ str'
  on flagExtended $ do
    c <- fromIntegral <$> word32BE
    count c extensionPair'
  return ""

requestId :: Parser RequestId
requestId = RequestId <$> word32BE

----------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------

newtype RequestId = RequestId Word32
  deriving Show

data Packet =
    FxpInit Word32
  -- ^ version, first message sent by the client
  | FxpVersion Word32 [(ByteString, ByteString)]
  -- ^ version, first message sent by the server
  | FxpOpen RequestId Text Word32 Attrs
  -- ^ filename, desired-access, flags, attrs. Open a file. The response is
  -- either FxpHandle (success) or FxpStatus (failure, e.g. the filename is
  -- actually a directory).
  | FxpClose RequestId ByteString -- TODO Use a newtype wrapper.
  -- ^ Close a handle (either from a file or a directory). Response is
  -- FxpStatus.
  | FxpRead ByteString
  | FxpWrite RequestId ByteString Word64 ByteString
  | FxpLStat RequestId Text
  | FxpFStat ByteString
  | FxpSetStat RequestId Text Attrs
  | FxpFSetStat RequestId ByteString Attrs
  | FxpOpenDir RequestId Text
  -- ^ dirname. A handle returned by FxpOpenDir is required to enumerate a
  -- directory.
  | FxpReadDir ByteString
  | FxpRemove ByteString
  | FxpMkDir RequestId Text Attrs
  | FxpRmDir ByteString
  | FxpRealPath RequestId Text (Maybe (Word8, [Text]))
  -- original-path, optional control-byte, compose-path.
  | FxpStat RequestId Text
  | FxpRename ByteString
  | FxpReadLink ByteString

  | FxpLink ByteString
  | FxpBlock ByteString
  | FxpUnblock ByteString

  | FxpStatus RequestId Word32 Text Text ByteString
  -- ^ code, message, language tag, data.
  | FxpHandle RequestId ByteString
  -- ^ Return a handle (an opaque string) to identify an open file or directory.
  | FxpData ByteString
  | FxpName RequestId [(Text, Attrs)] (Maybe Bool)
  | FxpAttrs RequestId Attrs

  | FxpExtended ByteString
  | FxpExtendedReply ByteString
  | Unknown ByteString
  deriving Show

data Attrs = Attrs
  { attrsSize :: Maybe Word64
  , attrsUidGid :: Maybe (Word32, Word32)
  , attrsPermission :: Maybe Word32
  , attrsAccessTime :: Maybe (Word32, Word32)
  , attrsExtended :: [(ByteString, ByteString)]
  }
  deriving Show

----------------------------------------------------------------------
-- Constants and flags
----------------------------------------------------------------------

flagSize, flagUidGid, flagPermissions, flagAccessTime, flagCreateTime,
  flagModifyTime, flagAcl, flagOwnerGroup, flagSubSecondTimes, flagBits,
  flagAllocationSize, flagTextHint, flagMimeType, flagLinkCount,
  flagUntranslatedName, flagCTime, flagExtended :: Word32

flagSize = 0x00000001
flagUidGid = 0x00000002 -- protocol 3
flagPermissions = 0x00000004
flagAccessTime = 0x00000008 -- AcModTime in protocol 3
flagCreateTime = 0x00000010
flagModifyTime = 0x00000020
flagAcl = 0x00000040
flagOwnerGroup = 0x00000080
flagSubSecondTimes = 0x00000100
flagBits = 0x00000200
flagAllocationSize = 0x00000400
flagTextHint = 0x00000800
flagMimeType = 0x00001000
flagLinkCount = 0x00002000
flagUntranslatedName = 0x00004000
flagCTime = 0x00008000
flagExtended = 0x80000000

contains :: Bits a => a -> a -> Bool
contains flags flag = flags .&. flag /= 0

----------------------------------------------------------------------
-- Basic parsers
----------------------------------------------------------------------

extensionPairs :: Int -> Parser [(ByteString, ByteString)]
extensionPairs n =
  if n <= 0
  then return []
  else do
    p <- extensionPair n
    ps <- extensionPairs $ n - 8 - BC.length (fst p) - BC.length (snd p)
    return $ p : ps

extensionPair :: Int -> Parser (ByteString, ByteString)
extensionPair n = do
  a <- str n
  b <- str $ n - 4 - BC.length a
  return (a, b)

extensionPair' :: Parser (ByteString, ByteString)
extensionPair' = do
  a <- str'
  b <- str'
  return (a, b)

strs :: Int -> Parser [ByteString]
strs n =
  if n <= 0
  then return []
  else do
    s <- str n
    ss <- strs $ n - 4 - BC.length s
    return $ s : ss

str :: Int -> Parser ByteString
str n = do
  when (n < 4) $ fail "Invalid string length."
  l <- fromIntegral <$> word32BE
  when (n - 4 < l) $ fail "Invalid remaining size while reading string."
  AB.take l

str' :: Parser ByteString
str' = do
  l <- fromIntegral <$> word32BE
  AB.take l

word32BE :: Parser Word32
word32BE = do
  w1 <- anyWord8
  w2 <- anyWord8
  w3 <- anyWord8
  w4 <- anyWord8
  return $!
    (fromIntegral w4 :: Word32) +
    fromIntegral w3 `unsafeShiftL` 8 +
    fromIntegral w2 `unsafeShiftL` 16 +
    fromIntegral w1 `unsafeShiftL` 24

word64BE :: Parser Word64
word64BE = do
  w1 <- word32BE
  w2 <- word32BE
  return $!
    (fromIntegral w2 :: Word64) +
    fromIntegral w1 `unsafeShiftL` 32
