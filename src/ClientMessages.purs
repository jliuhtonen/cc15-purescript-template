module ClientMessages where

import Data.Argonaut.Core
import Data.Argonaut.Decode
import Data.Argonaut.Encode
import Data.Argonaut.Combinators
import Data.Either
import Data.List
import Data.Maybe
import Prelude

newtype Connect = Connect {
  name :: String,
  gameId :: Maybe String
  }

instance encodeConnect :: EncodeJson Connect where
  encodeJson (Connect c) =
    "type"    := "connect" ~>
    "name"    := c.name ~>
    "game-id" := (encodeJson c.gameId) ~>
    jsonEmptyObject

newtype Pong = Pong Int

instance encodePong :: EncodeJson Pong where
  encodeJson (Pong connectionId) =
    "type"          := "pong" ~>
    "connection-id" := connectionId ~>
    jsonEmptyObject

newtype Join = Join {
  connectionId :: String,
  name :: String,
  color :: String,
  gameId :: String,
  players :: List Player
  }

instance encodeJoin :: EncodeJson Join where
  encodeJson (Join join) =
    "type" := "join" ~>
    "connection-id" := join.connectionId ~>
    "name" := join.name ~>
    "color" := join.color ~>
    "game-id" := join.gameId ~>
    "players" := encodeJson join.players ~>
    jsonEmptyObject

newtype Player = Player {
  number :: Int,
  name   :: String
  }

instance encodePlayer :: EncodeJson Player where
  encodeJson (Player player) =
    "number" := player.number ~>
    "name" := player.name ~>
    jsonEmptyObject
