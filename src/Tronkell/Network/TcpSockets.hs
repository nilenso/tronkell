module Tronkell.Network.TcpSockets where

import qualified Tronkell.Server.Types as ST
import qualified Control.Concurrent as Con
import qualified Tronkell.Network.Utils as NU (nextUserID)

import Network (PortID(..), accept, listenOn, withSocketsDo, Socket)
import System.IO
import qualified Control.Concurrent as C (forkIO)
import Control.Exception (handle, SomeException(..))
import Control.Monad (forever, when)
import qualified Data.Text as T (pack)

start :: Con.MVar ST.UserID -> ST.NetworkChans -> Con.Chan ST.OutMessage -> IO ()
start uIdGen chans outChan = withSocketsDo $ do
  sock <- listenOn . PortNumber $ 4242
  tcpMainLoop uIdGen chans outChan sock

tcpMainLoop :: Con.MVar ST.UserID -> ST.NetworkChans -> Con.Chan ST.OutMessage -> Socket -> IO ()
tcpMainLoop uIdGen chans outChan socket = do
  (clientHdl, _, _) <- accept socket
  hSetBuffering clientHdl NoBuffering
  userId <- NU.nextUserID uIdGen
  C.forkIO $ runClient userId clientHdl chans outChan
  tcpMainLoop uIdGen chans outChan socket

runClient :: ST.UserID -> Handle -> ST.NetworkChans -> Con.Chan ST.OutMessage -> IO ()
runClient uId clientHdl (inChan, clientSpecificOutChan) outChan = do
  -- duplicate the channels to read.
  dupOutChan <- Con.dupChan outChan
  dupClientSpecificOutChan <- Con.dupChan clientSpecificOutChan

  Con.writeChan inChan $ ST.PlayerJoined uId

  writeThread <- Con.forkIO $ forever $ do
    msg <- Con.readChan dupOutChan
    hPutStrLn clientHdl $ encodeMsg msg

  clientSpecificWriteThread <- Con.forkIO $ forever $ do
    (userId, msg) <- Con.readChan dupClientSpecificOutChan
    when (userId == uId) $ hPutStrLn clientHdl $ encodeMsg msg

  handle (\(SomeException _) -> return ()) $ forever $ do
    canRead <- hIsReadable clientHdl
    when canRead $ do
        inmsg <- cleanString <$> hGetLine clientHdl
        Con.writeChan inChan $ decodeMessage uId inmsg

  Con.writeChan inChan $ ST.UserExit uId
  Con.killThread writeThread
  Con.killThread clientSpecificWriteThread

cleanString :: String -> String
cleanString = reverse . dropWhile (\c -> c == '\n' || c == '\r') . reverse

decodeMessage :: ST.UserID -> String -> ST.InMessage
decodeMessage userId msg = case msg of
  "L"     -> ST.PlayerTurnLeft userId
  "R"     -> ST.PlayerTurnRight userId
  "Q"     -> ST.PlayerExit userId
  "ready" -> ST.PlayerReady userId
  n       -> ST.PlayerName userId $ T.pack n

encodeMsg :: ST.OutMessage -> String
encodeMsg = show
