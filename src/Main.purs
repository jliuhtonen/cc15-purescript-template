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

import MessageHandling 
import LoggingUtils
import qualified Game as Game

import Prelude

-- Config start
lobbyServer = testServer
teamName = "Rangers"
color = "purple"
initialGameId = Just "benders-game"
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
  let appContext = createAppContext socket
  AConsole.log $ show addrInfo
  liftEff $ (runST (runServer socket appContext))
  liftEff $ runReaderT (Game.connect teamName initialGameId) appContext

runServer :: forall h eff. UDP.Socket -> MsgHandlerContext -> Eff (st :: ST h, console :: CONSOLE, socket :: UDP.SOCKET | eff) Unit
runServer socket context = do
  serverState <- newSTRef Game.initialState
  let msgListener = createMessageDispatcher socket context serverState
  runAff logError logListenStart $ UDP.onMessage msgListener socket

createMessageDispatcher :: forall h eff. UDP.Socket -> MsgHandlerContext -> STRef h Game.State -> UDP.MessageListener (console :: CONSOLE, socket :: UDP.SOCKET, st :: ST h | eff)
createMessageDispatcher socket context serverState = \buf rinfo -> do
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
  lobbyServer: lobbyServer  
}
