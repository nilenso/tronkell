module Tronkell.Network.Utils where

import qualified Tronkell.Server.Types as ST
import qualified Control.Concurrent as Con

nextUserID :: Con.MVar ST.UserID -> IO (ST.UserID)
nextUserID userId =
  Con.modifyMVar userId $ \uId ->
      let
        next = ST.UserID $ 1 + ST.getUserID uId
      in
        return (next, next)
