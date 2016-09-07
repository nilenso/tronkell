{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Tronkell.Data.Parse where

import Tronkell.Types as T
import Tronkell.Game.Types as GT
import Tronkell.Server.Types as ST
import Data.Text as Text

import Data.Aeson as A (ToJSON, object, (.=), toJSON, Value(..))

instance ToJSON T.Coordinate where
  toJSON (x,y) = A.object ["x" .= x, "y" .= y]

instance ToJSON T.Orientation where
  toJSON = toStringValue

instance ToJSON GT.GameConfig where
  toJSON (GameConfig w h playerSpeed ticksPerSec) =
    A.object [ "width"            .= w
             , "height"           .= h
             , "player-speed"     .= playerSpeed
             , "ticks-per-second" .= ticksPerSec ]

instance ToJSON PlayerNick where
  toJSON (PlayerNick nick) = toStringValue nick

instance ToJSON PlayerStatus where
  toJSON = toStringValue

instance ToJSON Player where
  toJSON (Player nick status coord orient trail) =
    A.object [ "userid"      .= nick
             , "nick"        .= nick
             , "status"      .= status
             , "coordinate"  .= coord
             , "orientation" .= orient
             , "trail"       .= trail ]

instance ToJSON UserID where
  toJSON = A.String . getUserID

instance ToJSON OutMessage where
  toJSON msg =
    case msg of
      ST.GameReady config players ->
        A.object [ "type"    .= toStringValue "GameReady"
                 , "config"  .= config
                 , "players" .= players
                 ]
      ST.PlayerMoved uId coord orien ->
        A.object [ "type"        .= toStringValue "PlayerMoved"
                 , "userid"      .= uId
                 , "coordinate"  .= coord
                 , "orientation" .= orien
                 ]
      ST.PlayerDied uId coord ->
        A.object [ "type"       .= toStringValue "PlayerDied"
                 , "userid"     .= uId
                 , "coordinate" .= coord
                 ]
      ST.GameEnded winnerId ->
        A.object [ "type"     .= toStringValue "GameEnded"
                 , "winnerId" .=
                   case winnerId of
                     Just wId -> A.toJSON wId
                     Nothing  -> Null]
      ServerMsg msg -> toStringValue msg


toStringValue :: (Show a) => a -> Value
toStringValue = A.String . Text.pack . show
