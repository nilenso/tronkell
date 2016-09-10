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
import Control.Monad (forever)

import qualified Tronkell.Server.Types as ST
import qualified Tronkell.Data.Parse as TP

start :: Con.Chan ST.InMessage -> Con.Chan ST.OutMessage -> IO ()
start inChan outChan = Warp.runSettings
  (Warp.setPort 8331 Warp.defaultSettings)
  $ WaiWS.websocketsOr WS.defaultConnectionOptions (websocketHandler inChan outChan) staticApp

staticApp :: Network.Wai.Application
staticApp = Static.staticApp . Static.defaultWebAppSettings $ "ui/public"

websocketHandler :: Con.Chan ST.InMessage -> Con.Chan ST.OutMessage -> WS.ServerApp
websocketHandler inChan outChan pendingConnection = do
  conn <- WS.acceptRequest pendingConnection
  WS.forkPingThread conn 30
  -- get from some mvar passed from server to here.
  let userId = ST.UserID 1

  Con.writeChan inChan $ ST.PlayerJoined userId

  readThread <- Con.forkIO $ forever $ do
    msg <- Con.readChan outChan
    WS.sendDataMessage conn . WS.Binary . A.encode $ msg

  handle (\(SomeException _) -> return ()) $ forever $ do
    json <- toByteString <$> WS.receiveDataMessage conn
    let msg = A.eitherDecode' json
    case msg :: Either String TP.JsonInMessage of
      Left _  -> return ()
      Right m -> Con.writeChan inChan (m userId)

  Con.killThread readThread

toByteString :: WS.DataMessage -> ByteString
toByteString d = case d of
  WS.Binary b -> b
  WS.Text   t -> t
