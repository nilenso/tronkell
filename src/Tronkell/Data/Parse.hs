{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Tronkell.Data.Parse where

import Tronkell.Types as T
import Tronkell.Game.Types as GT
import Tronkell.Server.Types as ST
import Data.Text as Text

import qualified Data.Aeson as A
import Data.Aeson.Types as AT ((.=), (.:), typeMismatch)

instance A.ToJSON T.Coordinate where
  toJSON (x,y) = A.object [ "x" .= x, "y" .= y]

instance A.ToJSON T.Orientation where
  toJSON = A.String . Text.pack . show

instance A.ToJSON GT.GameConfig where
  toJSON (GameConfig w h playerSpeed ticksPerSec) =
    A.object [ "width"            .= w
             , "height"           .= h
             , "player-speed"     .= playerSpeed
             , "ticks-per-second" .= ticksPerSec ]

instance A.ToJSON PlayerNick where
  toJSON (PlayerNick nick) = A.String nick

instance A.ToJSON PlayerId where
  toJSON = A.Number . fromInteger . toInteger . getPlayerId

instance A.ToJSON PlayerStatus where
  toJSON = A.String . Text.pack . show

instance A.ToJSON Player where
  toJSON (Player pid nick status coord orient trail) =
    A.object [ "id"          .= pid
             , "nick"        .= nick
             , "status"      .= status
             , "coordinate"  .= coord
             , "orientation" .= orient
             , "trail"       .= trail ]

instance A.ToJSON UserID where
  toJSON = A.Number . fromInteger . toInteger . getUserID

instance A.ToJSON OutMessage where
  toJSON msg =
    case msg of
      ST.GameReady config players ->
        A.object [ "type"    .= A.String "GameReady"
                 , "config"  .= config
                 , "players" .= players
                 ]
      ST.PlayerMoved uId coord orien ->
        A.object [ "type"        .= A.String "PlayerMoved"
                 , "id"          .= uId
                 , "coordinate"  .= coord
                 , "orientation" .= orien
                 ]
      ST.PlayerDied uId coord ->
        A.object [ "type"       .= A.String "PlayerDied"
                 , "id"         .= uId
                 , "coordinate" .= coord
                 ]
      ST.GameEnded winnerId ->
        A.object [ "type"     .= A.String "GameEnded"
                 , "winnerId" .=
                   case winnerId of
                     Just wId -> A.toJSON wId
                     Nothing  -> A.Null
                 ]
      ServerMsg m ->
        A.object [ "type" .= A.String "ServerMsg"
                 , "message" .= m
                 ]
      PlayerRegisterId uId ->
        A.object [ "type" .= A.String "PlayerRegisterId"
                 , "id"   .= uId
                 ]

type JsonInMessage = UserID -> InMessage

instance A.FromJSON JsonInMessage where
  parseJSON (A.Object v) = do
    objType <- v .: "type"
    case objType of
      "Ready"  -> return PlayerReady
      "Exit"   -> return PlayerExit
      "Left"   -> return PlayerTurnLeft
      "Right"  -> return PlayerTurnRight
      "Name"   -> do
        name <- v .: "name"
        return (flip PlayerName name)
      j        -> AT.typeMismatch "InMessage" j -- should be wrong data error.

  parseJSON invalid = AT.typeMismatch "InMessage" invalid
