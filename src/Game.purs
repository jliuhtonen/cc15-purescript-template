module Game where

import Control.Monad.Eff.Console
import Data.Maybe
import Data.List
import Data.Foldable (traverse_, find)
import Data.Argonaut.Encode
import Data.Argonaut.Decode
import Control.Monad.Reader.Trans (lift, ask)
import qualified Node.Datagram as UDP
import Prelude

import LoggingUtils (logError)
import MessageHandling
import ServerMessages
import ClientMessages

type Server = {
  address :: UDP.Address,
  port :: UDP.Port  
}

type Game = {
  gameId :: String,
  server :: Server   
}

newtype State = State {
  connectionId :: Maybe Int,
  game :: Maybe Game
}

initialState = State { 
  connectionId: Nothing,
  game: Nothing
}

teamName = "Rangers"
teamColor = "purple"

players :: Array Player
players = [
  Player { number: 1, name: "Simon" },
  Player { number: 2, name: "Erik"  },
  Player { number: 3, name: "Paul" },
  Player { number: 4, name: "Philip" }
]

handleIncomingMsg :: forall e. UDP.RemoteAddressInfo -> State -> ServerMessage -> AppMsgHandler (console :: CONSOLE, socket :: UDP.SOCKET | e) State

handleIncomingMsg sender (State state) (Ping) = do
  appLog "Sending pong"
  traverse_ (\connId -> sendMessage (Pong connId) sender) state.connectionId
  pure $ State state

handleIncomingMsg _ (State state) (ConnectOk newConnId) = do
  appLog $ "Connection to lobby established with id " ++ (show newConnId)
  let updatedState = state { connectionId = Just newConnId } 
  pure $ State updatedState

handleIncomingMsg _ state (Goodbye) = do
  appLog "Got goodbye, terminating"
  closeSocket
  pure state

handleIncomingMsg _ (State state) (GameAt (GameObject { address: address, port: port, gameId: gameId })) = do
  appLog $ "Joining game " ++ gameId ++ " at " ++ address ++ ":" ++ show port
  traverse_ (joinGame address port gameId) state.connectionId
  pure $ State state

joinGame :: forall e. String -> Int -> String -> Int -> AppMsgHandler (console :: CONSOLE, socket :: UDP.SOCKET | e) Unit
joinGame address port gameId connectionId = sendMessage joinMsg gameServer where
  gameServer = toMessageReceiver address port
  joinMsg = Join {
    connectionId: connectionId,
    name: teamName,
    color: teamColor,
    gameId: gameId,
    players: players
  } 

connect :: String -> Maybe String -> AppMsgHandler (console :: CONSOLE, socket :: UDP.SOCKET) Unit
connect name gameId = do
  MsgHandlerContext { lobbyServer: lobbyServer } <- ask
  appLog $ "Connecting to " ++ (show gameId)
  let connect = Connect { name: name, gameId: gameId } 
  sendMessage connect lobbyServer

appLog :: forall e. String -> AppMsgHandler (console :: CONSOLE | e) Unit
appLog = lift <<< log
