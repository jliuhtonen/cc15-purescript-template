module Bot where

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

type Game = {
  server :: UDP.RemoteAddressInfo,
  gameId :: String
}

newtype State = State {
  connectionId :: Maybe Int,
  game :: Maybe Game
}

initialState = State { 
  connectionId: Nothing,
  game: Nothing
}

handleIncomingMsg :: forall e. UDP.RemoteAddressInfo -> State -> ServerMessage -> BotMsgHandler (console :: CONSOLE, socket :: UDP.SOCKET | e) State

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
  let server = UDP.RemoteAddressInfo { address: address, port: port }
  traverse_ (joinGame server gameId) state.connectionId
  let game = { gameId: gameId, server: server }
  let updatedState = state { game = Just game }
  pure $ State updatedState

handleIncomingMsg _ state (JoinOk) = do
  appLog "Successfully joined game!"
  pure state

handleIncomingMsg _ (State state) (JoinError err) = do
  appLog $ "Error joining game: " ++ err
  let updatedState = state { game = Nothing }
  pure $ State updatedState

joinGame :: forall e. UDP.RemoteAddressInfo -> String -> Int -> BotMsgHandler (console :: CONSOLE, socket :: UDP.SOCKET | e) Unit
joinGame server gameId connectionId = do
  MsgHandlerContext { teamName: teamName, teamColor: teamColor, players: players } <- ask
  let joinMsg = Join { connectionId: connectionId, name: teamName, color: teamColor,
                        gameId: gameId, players: players } 
  sendMessage joinMsg server

connect :: String -> Maybe String -> BotMsgHandler (console :: CONSOLE, socket :: UDP.SOCKET) Unit
connect name gameId = do
  MsgHandlerContext { lobbyServer: lobbyServer } <- ask
  appLog $ "Connecting to " ++ (show gameId)
  let connect = Connect { name: name, gameId: gameId } 
  sendMessage connect lobbyServer

appLog :: forall e. String -> BotMsgHandler (console :: CONSOLE | e) Unit
appLog = lift <<< log
