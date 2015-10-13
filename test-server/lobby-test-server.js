#!/usr/bin/env node

/*

*** CODECAMP 2015 SIMPLE LOBBY TEST SERVER ***

Usage: 

$ node lobby-test-server.js

Listens to incoming UDP connections in port 4567

Note: does not validate eg. pong replies

Example output:
---------------

  Bot >> Lobby {"type":"connect","name":"Planet Express","game-id":"bendersgame"}
  Connection established to 127.0.0.1:4652, connection-id 1391
  Lobby >> Bot {"type":"connect-ok","connection-id":1391}
  Lobby >> Bot {"type":"ping"}
  Bot >> Lobby {"type":"pong","connection-id":1391}
  Lobby >> Bot {"type":"game-at","address":"0.0.0.0","port":7654,"game-id":"bendersgame"}
  Bot >> Game {"type":"join","name":"Planet Express","color":"red","players":[{"number":1,"name":"Fry"},{"number":2,"name":"Leela"}],"game-id":"bendersgame","connection-id":1391}
  POW! Successful game join by 127.0.0.1:4652, connection-id 1391

*/

var dgram = require("dgram")

var LOBBY_PORT = 4567
var GAME_PORT = 7654

var lobbySocket = dgram.createSocket('udp4')
lobbySocket.bind(LOBBY_PORT)
var gameSocket = dgram.createSocket('udp4')
gameSocket.bind(GAME_PORT)

function randomConnectionId() {
  return Math.round(Math.random() * 10000)
}

var connections = {}

gameSocket.on('message', function(rawMsg, rinfo) {
  console.info('Bot >> Game', rawMsg.toString())
  var msg = JSON.parse(rawMsg.toString())
  if (msg.type !== 'join') {
    console.warn('Wrong message type, expecting "join" but was', msg.type)
    return
  }
  if (!msg['connection-id']) {
    console.warn('Missing connection id')
    return
  }
  if (!msg.name && typeof msg.name !== "string") {
    console.warn('Invalid name in join')
    return
  }
  if (msg['game-id'] && msg['game-id'] !== 'bendersgame') {
    console.warn('Invalid game-id in join')
    return
  }
  if (!msg.players) {
    console.warn('Players data not present in join')
    return
  }
  console.info('POW! Successful game join by ' + rinfo.address + ':' + rinfo.port + ', connection-id ' + msg['connection-id'])
})


lobbySocket.on('message', function(msg, rinfo) {
  console.info('Bot >> Lobby', msg.toString())
  var parsed = JSON.parse(msg.toString())
  if (!parsed.type) {
    console.warn('Message type undefined')
  } else if (parsed.type === 'connect' && validateConnect(parsed)) {
    var id = randomConnectionId()
    var connection = new LobbyConnection(lobbySocket, id, rinfo.address, rinfo.port)
    connections[id] = connection
  } else {
    var connectionId = parsed['connection-id']
    if (!connectionId) {
      console.error('No connection id present')
      return
    }
    if (!connections[connectionId]) {
      console.warn('No connection present')
      return
    }
  }
})

function LobbyConnection(socket, connectionId, address, port) {
  function send(msg) {
    var msgStr = JSON.stringify(msg)
    console.info('Lobby >> Bot', msgStr)
    socket.send(new Buffer(msgStr), 0, msgStr.length, port, address)
  }

  console.info('Connection established to ' + address + ':' + port + ', connection-id ' + connectionId)
  send({ type: 'connect-ok', 'connection-id': connectionId })

  function ping() {
    send({type: 'ping'})
  }

  function gameAt() {
    send({ "type": "game-at", "address": gameSocket.address().address , "port": gameSocket.address().port, "game-id": "bendersgame"})
  }

  setTimeout(ping, 1000)
  setTimeout(gameAt, 1500)
}

function validateConnect(msg) {
  if (!msg.name && typeof msg.name !== "string") {
    console.warn('Invalid name in connect')
    return false
  }
  return true
}
