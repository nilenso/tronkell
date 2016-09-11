{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module TestServer where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Data.ByteString.Char8 as C (pack)
import qualified Data.Map as M
import System.IO
import Control.Concurrent
import Control.Concurrent.STM

import Tronkell.Server.Types as STypes
import Tronkell.Server.Server as SServer
import Tronkell.Game.Types as GTypes

instance Show Server where
  show Server{..} = "Server: " ++ show serverGameConfig

genServer = do
    let conf = GTypes.GameConfig 100 100 1 1
    users <- newMVar M.empty
    networkInChan         <- newChan
    clientSpecificOutChan <- newChan
    serverChan <- atomically newTChan
    clientsChan <- newChan
    internalChan <- newChan
    return $ Server conf users (networkInChan, clientSpecificOutChan) serverChan clientsChan internalChan

main :: IO ()
main = hspec $
  describe "runClient : " $ do
    it "should take user name first" $
      property $ monadicIO $ do
        clientChan <- run $ atomically newTChan
        let uId = UserID 0
        run $ atomically $ writeTChan clientChan (PlayerName uId "ashish")
        run $ atomically $ writeTChan clientChan (PlayerExit uId)
        run $ atomically $ writeTChan clientChan (PlayerName uId "bad-name")
        server <- run genServer
        run $ runClient uId clientChan server
        users <- run $ readMVar (serverUsers server)
        assert $ length users == 1 &&
                 Just "ashish" == (userNick . head . M.elems $ users)

    -- since now names do not need to be unique - do we need this functionaliy ?
    -- maybe yes.. for better user experience..
    it "should ask again for user-name if already taken" $
      property $ monadicIO $ do
        clientChan <- run $ atomically newTChan
        let uId  = UserID 1
            takenName = "ashish1"
            uId2 = UserID 2
            user = User uId2 (Just takenName) Waiting

        run $ atomically $ writeTChan clientChan (PlayerName uId takenName)
        run $ atomically $ writeTChan clientChan (PlayerName uId "ashish2")
        run $ atomically $ writeTChan clientChan (PlayerExit uId)

        server <- run genServer
        -- uId2 has taken same name as "takenName"
        run $ modifyMVar_ (serverUsers server) $ \users -> return $ M.insert uId2 user users
        run $ runClient uId clientChan server
        users <- run $ readMVar (serverUsers server)
        assert $ length users == 2 &&
                 [Just "ashish2", Just "ashish1"] == (userNick <$> M.elems users)
