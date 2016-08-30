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

import MockSocket as MSocket

instance Show Server where
  show Server{..} = "Server: " ++ show serverGameConfig

genServer = do
    let conf = GTypes.GameConfig 100 100 1 1
    users <- newMVar M.empty
    serverChan <- atomically newTChan
    clientsChan <- newChan
    internalChan <- newChan
    return $ Server conf users undefined serverChan clientsChan internalChan

genHandle inputString = do
  knob <- MSocket.newMockSocket $ C.pack inputString
  newFileHandle knob "tmp.txt" ReadWriteMode

main :: IO ()
main = hspec $
  describe "runClient : " $ do
    it "should take user name first" $
      property $ monadicIO $ do
        clientHandle <- genHandle "username\r\nquit\r\nmore-random-input\r\n"
        server <- run genServer
        run $ runClient clientHandle server
        users <- run $ readMVar (serverUsers server)
        assert $ length users == 1 &&
                 "username" == (getUserID . userId . head . M.elems $ users)

    it "should ask again for user-name if already taken" $
      property $ monadicIO $ do
        clientHandle <- genHandle "Username1\r\nUsername2\r\nquit\r\n"
        server <- run genServer
        let uId  = UserID "Username1"
            user = User uId (Just "Username1") Waiting
        run $ modifyMVar_ (serverUsers server) $ \users -> return $ M.insert uId user users
        run $ runClient clientHandle server
        users <- run $ readMVar (serverUsers server)
        assert $ length users == 2 &&
                 ["Username1", "Username2"] == (getUserID . userId <$> M.elems users)
