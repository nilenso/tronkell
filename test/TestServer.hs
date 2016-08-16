{-# LANGUAGE RecordWildCards #-}

module TestServer where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Data.ByteString.Char8 as C (pack)
import Data.Text as T (pack, unpack)
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
    users <- newMVar []
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
      property $ \clientId -> monadicIO $ do
        clientHandle <- genHandle "username\r\nquit\r\nmore-random-input\r\n"
        server <- run genServer
        run $ runClient clientHandle server clientId
        users <- run $ readMVar (serverUsers server)
        assert $ length users == 1 && T.pack "username" == (getUserID . userId . head $ users)

    it "should ask again for user-name if already taken" $
      property $ \clientId -> monadicIO $ do
        clientHandle <- genHandle "Username1\r\nUsername2\r\nquit\r\n"
        server <- run genServer
        run $ modifyMVar_ (serverUsers server) $ \users -> return $ SServer.mkUser "Username1" : users
        run $ runClient clientHandle server clientId
        users <- run $ readMVar (serverUsers server)
        assert $ length users == 2 && ["Username2", "Username1"] == map (T.unpack . getUserID . userId) users
