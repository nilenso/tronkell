{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Tronkell.Game.Types
import Tronkell.Game.Engine

import qualified Data.Map as Map
import Data.List
import System.Random
import Test.Hspec
import Test.QuickCheck

newtype BoundedInt = BoundedInt { unBoundedInt :: Int } deriving (Show, Eq, Ord, Num)

instance Random BoundedInt where
  randomR (BoundedInt lo, BoundedInt hi) g =
    let (a, g') = randomR (lo, hi) g in (BoundedInt a, g')

  random g = let (a, g') = random g in (BoundedInt a, g')

instance Arbitrary BoundedInt where
  arbitrary = fmap BoundedInt $ choose (0, 2)


instance Arbitrary GameConfig where
  arbitrary = do
    w <- unBoundedInt <$> arbitrary
    h <- unBoundedInt <$> arbitrary
    sp <- unBoundedInt <$> arbitrary
    return $ GameConfig w h sp 1


instance Arbitrary Player where
  arbitrary = do
    n <- PlayerNick <$> arbitrary
    x <- unBoundedInt <$> arbitrary
    y <- unBoundedInt <$> arbitrary
    o <- arbitraryBoundedEnum
    return $ Player n Alive (x, y) o []


isValidPlayer :: GameConfig -> Player -> Bool
isValidPlayer (GameConfig w h _ _) (Player _ _ (x, y) _ _) = x < w && y < h

areOverlappingPlayers :: Player -> Player -> Bool
areOverlappingPlayers p1 p2 = playerCoordinate p1 == playerCoordinate p2

instance Arbitrary Game where
  arbitrary = do
    np   <- unBoundedInt <$> arbitrary
    conf <- arbitrary
    ps   <- nubBy areOverlappingPlayers <$> (vectorOf np $ arbitrary `suchThat` isValidPlayer conf)
    let mp = Map.fromList . map (\p -> (playerNick p, p)) $ ps
    return $ Game Nothing mp InProgress conf


isPlayerOnGrid :: Game -> Player -> Bool
isPlayerOnGrid game p = isValidPlayer (gameConfig game) $ p

main :: IO ()
main = hspec $ do
  describe "game" $ do
    it "has all players on grid" $
      property $ \game -> and . Map.map (isPlayerOnGrid game) $ (gamePlayers game)
