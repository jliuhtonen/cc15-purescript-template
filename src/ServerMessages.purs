module ServerMessages where

import Data.Argonaut.Core
import Data.Argonaut.Decode
import Data.Argonaut.Encode
import Data.Argonaut.Combinators
import Data.Either
import Prelude

data ServerMessage = 
  ConnectOk Int | 
  Goodbye | 
  Ping | 
  GameAt GameObject |
  JoinOk |
  JoinError String

instance decodeServerMessage :: DecodeJson ServerMessage where
  decodeJson json = do
    obj <- decodeJson json
    msgType <- obj .? "type"
    case msgType of
         "connect-ok" -> ConnectOk <$> obj .? "connection-id"
         "goodbye"    -> pure Goodbye
         "ping"       -> pure Ping
         "game-at"    -> GameAt <$> decodeJson json
         "join-ok"    -> pure JoinOk
         "join-error" -> JoinError <$> (obj .? "description")
         _            -> Left $ "Unknown type " ++ msgType

newtype GameObject = GameObject {
  address :: String,
  port    :: Int, 
  gameId  :: String 
}

instance decodeGameObject :: DecodeJson GameObject where
  decodeJson json = do
    obj <- decodeJson json
    address <- obj .? "address"
    port <- obj .? "port"
    gameId <- obj .? "game-id"
    pure $ GameObject {
        address: address,
        port: port,
        gameId: gameId  
      }
