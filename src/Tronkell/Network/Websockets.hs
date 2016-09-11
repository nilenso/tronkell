module Tronkell.Network.Websockets where

import qualified Network.WebSockets as WS
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai.Application.Static as Static
import qualified Network.Wai
import qualified Control.Concurrent as Con
import qualified Data.Aeson as A
import Data.ByteString.Lazy.Char8
import Control.Exception (handle, SomeException(..))
import Control.Monad (forever, when)
import Debug.Trace

import qualified Tronkell.Server.Types as ST
import qualified Tronkell.Data.Parse as TP

start :: Con.MVar ST.UserID -> ST.NetworkChans -> Con.Chan ST.OutMessage -> IO ()
start uIdGen chans outChan = Warp.runSettings
  (Warp.setPort 8331 Warp.defaultSettings)
  $ WaiWS.websocketsOr WS.defaultConnectionOptions (websocketHandler uIdGen chans outChan) staticApp

staticApp :: Network.Wai.Application
staticApp = Static.staticApp . Static.defaultWebAppSettings $ "ui/public"

websocketHandler :: Con.MVar ST.UserID -> ST.NetworkChans -> Con.Chan ST.OutMessage -> WS.ServerApp
websocketHandler uIdGen (inChan, clientSpecificOutChan) outChan pendingConnection = do
  conn <- WS.acceptRequest pendingConnection
  WS.forkPingThread conn 30
  -- duplicate the channels to read.
  dupOutChan <- Con.dupChan outChan
  dupClientSpecificOutChan <- Con.dupChan clientSpecificOutChan

  -- get from some mvar passed from server to here.
  userId <- nextUserID uIdGen
  Con.writeChan inChan $ ST.PlayerJoined userId

  writeThread <- Con.forkIO $ forever $ do
    msg <- Con.readChan dupOutChan
    WS.sendDataMessage conn . WS.Text . A.encode $ msg

  clientSpecificWriteThread <- Con.forkIO $ forever $ do
    (uId, msg) <- Con.readChan dupClientSpecificOutChan
    when (uId == userId) $ WS.sendDataMessage conn . WS.Text . A.encode $ msg

  handle (\(SomeException _) -> return ()) $ forever $ do
    json <- toByteString <$> WS.receiveDataMessage conn
    let msg = A.eitherDecode' json
    case msg :: Either String TP.JsonInMessage of
      Left e  -> traceShow e $ return ()
      Right m -> Con.writeChan inChan (m userId)

  Con.writeChan inChan $ ST.UserExit userId
  Con.killThread writeThread
  Con.killThread clientSpecificWriteThread

toByteString :: WS.DataMessage -> ByteString
toByteString d = case d of
  WS.Binary b -> b
  WS.Text   t -> t

nextUserID :: Con.MVar ST.UserID -> IO (ST.UserID)
nextUserID userId =
  Con.modifyMVar userId $ \uId ->
      let
        next = ST.UserID $ 1 + ST.getUserID uId
      in
        return (next, next)
