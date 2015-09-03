{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
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
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import qualified Data.Attoparsec.ByteString as AB (take)
import Data.Attoparsec.ByteString hiding (parse, take)
import Data.Bits (unsafeShiftL, (.&.), Bits)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BC
import Data.List (partition)
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

import Network.SFTP.Messages

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

  (sftpInp, sftpOut, sftpErr, _) <-
    runInteractiveProcess "/usr/lib/openssh/sftp-server-original"
      args Nothing Nothing

  (is, is') <- splitIS S.stdin
  (is2, is2') <- splitIS sftpOut

  inThread <- forkIO $ S.connect is' sftpInp
  errThread <- forkIO $ S.connect sftpErr S.stderr
  _ <- forkIO $ S.connect is2' S.stdout
  -- S.withFileAsOutput "/home/sftp/debug.txt" $ \observe -> do
  mvar <- newEmptyMVar
  _ <- forkIO $ do
    ps <- execWriterT (protocol is)
    putMVar mvar ps
  ps <- execWriterT (protocol is2)
  ps' <- takeMVar mvar
  S.withFileAsOutput "/home/sftp/debug.txt" $ \os -> do
    S.write (Just . BC.pack $ "Arguments: " ++ show args ++ "\n") os
    mapM_ (\(s, p) -> S.write (Just . BC.pack $ s ++ display p ++ "\n") os) $ merge ps' ps
  return ()
  -- takeMVar inThread
  -- takeMVar errThread

merge [] ys = map ("<<< ",) ys
merge (x:xs) ys = (">>> ", x) : ("<<< ", y') : merge xs ys'
  where (y', ys') = case partition ((== packetId x) . packetId) ys of
          (y:ys1, ys2) -> (y, ys1 ++ ys2)
          (_, ys2) -> (Unknown (RequestId 0) "Missing response.", ys2)

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

display :: Packet -> String
display p = short p ++ (let d = description p in if null d then "" else " " ++ d)
