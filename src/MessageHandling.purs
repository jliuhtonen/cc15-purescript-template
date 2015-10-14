module MessageHandling where

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console hiding (error)
import Control.Monad.Eff.Exception
import Control.Monad.Reader.Trans
import Data.Either
import Data.Argonaut.Core hiding (toString)
import Data.Argonaut.Parser
import Data.Argonaut.Printer
import Data.Argonaut.Decode
import Data.Argonaut.Encode
import qualified Node.Buffer as Buffer
import qualified Node.Encoding as Encoding
import qualified Node.Datagram as UDP

import Prelude

import LoggingUtils
import ServerMessages

newtype MsgHandlerContext = MsgHandlerContext {
  socket :: UDP.Socket,
  lobbyServer :: UDP.RemoteAddressInfo
}

type AppMsgHandler eff = ReaderT MsgHandlerContext (Eff eff)

toMessageReceiver :: String -> Int -> UDP.RemoteAddressInfo
toMessageReceiver address port = UDP.RemoteAddressInfo {
  address: address,
  port: port  
}

sendMessage :: forall a eff. (EncodeJson a) => a -> UDP.RemoteAddressInfo -> AppMsgHandler (socket :: UDP.SOCKET, console :: CONSOLE | eff) Unit
sendMessage msg (UDP.RemoteAddressInfo { address: address, port: port }) = do
  MsgHandlerContext { socket: socket } <- ask
  let jsonStr = printJson $ encodeJson msg
  let socketSendAction = sendStringToSocket socket address port jsonStr
  runSocketActionLoggingErrors socketSendAction

closeSocket :: forall eff. AppMsgHandler (socket :: UDP.SOCKET, console :: CONSOLE | eff) Unit
closeSocket = do
  MsgHandlerContext { socket: socket } <- ask
  runSocketActionLoggingErrors $ UDP.unref socket

runSocketActionLoggingErrors :: forall a eff. Aff (socket :: UDP.SOCKET, console :: CONSOLE | eff) a -> AppMsgHandler (socket :: UDP.SOCKET, console :: CONSOLE | eff) Unit
runSocketActionLoggingErrors action =
  lift <<< catchException logError $ launchAff action

sendStringToSocket :: forall eff. UDP.Socket -> UDP.Address -> UDP.Port -> String -> Aff (socket :: UDP.SOCKET | eff) Unit
sendStringToSocket socket address port msg =
  UDP.send buffer 0 (Buffer.size buffer) port address socket where
    buffer = Buffer.fromString msg Encoding.UTF8

parseIncomingMsg :: String -> Either String ServerMessage
parseIncomingMsg msg = jsonParser msg >>= (decodeJson :: Json -> Either String ServerMessage)
