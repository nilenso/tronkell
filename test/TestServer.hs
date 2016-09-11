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

-- rethink the tests.
main :: IO ()
main = return ()
