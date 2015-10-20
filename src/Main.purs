module Main where

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console
import Control.Monad.ST
import Control.Monad.Reader.Trans
import qualified Control.Monad.Aff.Console as AConsole
import Data.Either
import Data.List
import Data.Maybe
import qualified Node.Datagram as UDP
import qualified Node.Encoding as Encoding
import qualified Node.Buffer as Buffer

import ClientMessages
import MessageHandling 
import LoggingUtils
import qualified Game as Game

import Prelude

-- Config start
lobbyServer = testServer
initialGameId = Just "benders-game"
teamName = "Rangers"
teamColor = "purple"
players :: Array Player
players = [
  Player { number: 1, name: "Simon" },
  Player { number: 2, name: "Erik"  },
  Player { number: 3, name: "Paul" },
  Player { number: 4, name: "Philip" }
]
-- Config end

-- Networking settings start
socketType = UDP.UDP4
encoding = Encoding.UTF8
listenPort = Nothing -- random port given by OS
listenInterfaces = Nothing -- all interfaces
-- Networking settings end

testServer = UDP.RemoteAddressInfo {
  address: "127.0.0.1",
  port: 4567
}

main = launchAff $ do
  socket <- UDP.createSocket socketType
  UDP.onError logError socket
  addrInfo <- UDP.bindSocket listenPort listenInterfaces socket
  let botContext = createAppContext socket
  AConsole.log $ show addrInfo
  liftEff $ (runST (runBot socket botContext))
  liftEff $ runReaderT (Game.connect teamName initialGameId) botContext

runBot :: forall h eff. UDP.Socket -> MsgHandlerContext -> Eff (st :: ST h, console :: CONSOLE, socket :: UDP.SOCKET | eff) Unit
runBot socket context = do
  serverState <- newSTRef Game.initialState
  let msgListener = createMessageListener socket context serverState
  runAff logError logListenStart $ UDP.onMessage msgListener socket

createMessageListener :: forall h eff. UDP.Socket -> MsgHandlerContext -> STRef h Game.State -> UDP.MessageListener (console :: CONSOLE, socket :: UDP.SOCKET, st :: ST h | eff)
createMessageListener socket context serverState = \buf rinfo -> do
  let msg = Buffer.toString encoding buf
  let parsedJson = parseIncomingMsg msg
  case parsedJson of
       (Left errMsg) -> log $ "Error parsing JSON: " ++ errMsg
       (Right msg) -> do
         currentState <- readSTRef serverState
         let handler = Game.handleIncomingMsg rinfo currentState msg
         newState <- runReaderT handler context
         writeSTRef serverState newState
         pure unit

logListenStart :: forall eff. Unit -> Eff (console :: CONSOLE | eff) Unit 
logListenStart _ = log "Listening for connections..."

createAppContext :: UDP.Socket -> MsgHandlerContext
createAppContext socket = MsgHandlerContext {
  socket: socket,
  lobbyServer: lobbyServer,
  teamName: teamName,
  teamColor: teamColor,
  players: players
}
