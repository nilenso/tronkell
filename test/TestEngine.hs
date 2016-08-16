{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module TestEngine where

import Tronkell.Types
import Tronkell.Game.Types
import Tronkell.Game.Engine

import qualified Data.Map as Map
import Data.List (nub, nubBy)
import Data.Maybe (isNothing, fromJust)
import System.Random

import Test.Hspec
import Test.QuickCheck

newtype BoundedInt = BoundedInt { unBoundedInt :: Int }
                     deriving (Show, Eq, Ord, Num)

instance Random BoundedInt where
  randomR (BoundedInt lo, BoundedInt hi) g =
    let (a, g') = randomR (lo, hi) g in (BoundedInt a, g')

  random g = let (a, g') = random g in (BoundedInt a, g')

instance Arbitrary BoundedInt where
  arbitrary = BoundedInt <$> choose (0, 100)

instance Arbitrary GameConfig where
  arbitrary = do
    w  <- unBoundedInt <$> arbitrary `suchThat` (> 0)
    h  <- unBoundedInt <$> arbitrary `suchThat` (> 0)
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
areOverlappingPlayers p1 p2 =
  playerNick p1 == playerNick p2 || playerCoordinate p1 == playerCoordinate p2

genPlayer :: GameConfig -> Gen Player
genPlayer GameConfig{..} = do
  nick <- PlayerNick <$> arbitrary
  x    <- unBoundedInt <$> arbitrary `suchThat` (>= 0) `suchThat` (< BoundedInt gameWidth)
  y    <- unBoundedInt <$> arbitrary `suchThat` (>= 0) `suchThat` (< BoundedInt gameHeight)
  o    <- arbitraryBoundedEnum
  return $ Player nick Alive (x, y) o []

instance Arbitrary Game where
  arbitrary = do
    conf   <- arbitrary
    np     <- unBoundedInt
              <$> arbitrary `suchThat` (< (BoundedInt $ gameWidth conf * gameHeight conf))
                            `suchThat` (> 0)
    ps     <- nubBy areOverlappingPlayers
              <$> vectorOf np (genPlayer conf)
    let mp = Map.fromList . map (\p -> (playerNick p, p)) $ ps
    return $ Game Nothing mp InProgress conf

isPlayerOnGrid :: Game -> Player -> Bool
isPlayerOnGrid game = isValidPlayer (gameConfig game)

main :: IO ()
main = hspec $ do
  describe "game init setup" $ do
    it "has all players on grid" $
      property $ \game -> testPlayerProperty (isPlayerOnGrid game) game

    it "has no trails at the start" $
      property $ testPlayerProperty (null . playerTrail)

    it "has all alive players" $
      property $ testPlayerProperty ((== Alive) . playerStatus)

    it "has no overlapping players" $
      property $ \Game {..} ->
        length gamePlayers == (length . nub . Map.elems . Map.map playerCoordinate $ gamePlayers)

    it "has No winner at start" $
      property $ isNothing . gameWinner

    it "is in progress at start" $
      property $ (== InProgress) . gameStatus

  describe "game in progress" $ do
    it "has all players on grid after some events" $
      property $ \game nums ->
        let events = map (genEvent game) nums
            (_, game') = runEngine gameEngine game events
        in testPlayerProperty (isPlayerOnGrid game') game'

    it "changes player orientation on turn event" $
      property $ \game (Positive eventNo, Positive playerNo) ->
        let event = genEvent game (Positive (eventNo `mod` 2 + 1),
                                   Positive playerNo)
            nick = case event of
              TurnLeft nick -> nick
              TurnRight nick -> nick
              _ -> error "eventNo `mod` 2 + 1 made sure that we got one of above events."
            oldOrientation = getPlayerField playerOrientation nick game
            expectedOrientation = doTurn event oldOrientation
            (PlayerMoved nick' _ orientation' : _, game') = runEngine gameEngine game [event]
            actualOrientation = getPlayerField playerOrientation nick game'
        in expectedOrientation == actualOrientation &&
           nick == nick' &&
           actualOrientation == orientation'

    it "moves player by number of tick events" $
      property $ \game (Positive steps) ->
        let (_, game') = runEngine gameEngine game $ replicate steps Tick
            expectedPositions = Map.map (movePlayer steps (gameConfig game)) $ gamePlayers game
            actualPositions = getPlayersField playerCoordinate game'
        in (expectedPositions == actualPositions) || (gameStatus game' == Finished)

    it "changes player status to dead after he quits" $
      property $ \game@Game{..} (Positive playerNo) ->
        let nick = Map.keys gamePlayers !! (playerNo `mod` Map.size gamePlayers)
            playerOldCoord = getPlayerField playerCoordinate nick game
            (PlayerDied nick' coord' : _, game') = runEngine gameEngine game [PlayerQuit nick]
            playerNewStatus = getPlayerField playerStatus nick game'
        in playerNewStatus == Dead &&
           nick == nick' &&
           playerOldCoord == coord'

    it "Game finishes when players quit one by one" $
      property $ \game eventsAndPlayerNos ->
        if not . null $ eventsAndPlayerNos
        then let events = map (genEvent game) eventsAndPlayerNos
                 quitEvents = map PlayerQuit $ Map.keys (gamePlayers game)
                 allEvents = events ++ quitEvents
                 (_, game') = runEngine gameEngine game allEvents
             -- we can not check for last quitting player to be the winner,
             -- as because of the movements in Tick, he can also die earlier.
             -- what is guaranteed is that Game should finish.
             in gameStatus game' == Finished
        else True

  where
    testPlayerProperty f = and . Map.map f . gamePlayers

    genEvent :: Game -> (Positive Int, Positive Int) -> InputEvent
    genEvent Game {..} (Positive eventNo, Positive playerNo) =
      let nick = Map.keys gamePlayers !! (playerNo `mod` Map.size gamePlayers)
      in case eventNo `mod` 4 of
        0 -> Tick
        1 -> TurnLeft nick
        2 -> TurnRight nick
        3 -> PlayerQuit nick
        _ -> error "Should never happen"

    getPlayerField field nick game = field . fromJust . Map.lookup nick $ gamePlayers game

    doTurn event oldOrientation =
      case (oldOrientation, event) of
        (North, TurnLeft _ ) -> West
        (West, TurnLeft _) -> South
        (South, TurnLeft _) -> East
        (East, TurnLeft _) -> North
        (_, TurnRight x) ->
          let doLeftTurn = doTurn (TurnLeft x)
          in doLeftTurn . doLeftTurn . doLeftTurn $ oldOrientation

    getPlayersField field game = Map.map field $ gamePlayers game

    movePlayer steps GameConfig{..} player@Player{..} =
      let (x, y) = playerCoordinate
          newCoordinate = case playerOrientation of
            North -> (x, max (y - steps) 0)
            South -> (x, min (y + steps) (gameHeight - 1))
            West  -> (max (x - steps) 0, y)
            East  -> (min (x + steps) (gameWidth - 1), y)
      in newCoordinate
