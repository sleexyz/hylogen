"use strict";

const path = require("path");
const express = require("express");
const webpack = require("webpack");
const generateConfig = require("./generate-webpack.config");

const dev  = require("webpack-dev-middleware");
const hot = require("webpack-hot-middleware");

const osc = require("osc");
const ws = require("ws");



function runServer(entryPath, outputPath, publicPath, portNum) {
  const app = express();
  const config = generateConfig(entryPath, outputPath, publicPath, false);

  const compiler = webpack(config);

  app.use(dev(compiler, {
    noInfo: true,
    publicPath: config.output.publicPath
  }));

  app.use(hot(compiler));

  // app.use(express.static(outputPath));
  app.get("/", function(req, res) {
    res.sendFile(path.join(__dirname, outputPath + "/index.html"));
  });

  app.listen(portNum, "localhost", function (err) {
    if(err) {
      console.log(err);
      return;
    }
  });
}

function runOSC(udpPortNum, wsPortNum) {
  const udpPort = new osc.UDPPort({
    // localAddress: "0.0.0.0",
    localAddress: "127.0.0.1",
    localPort: udpPortNum
  });

  udpPort.on("bundle", function (oscBundle, timeTag, info) {
    console.log("An OSC bundle just arrived for time tag", timeTag, ":", oscBundle);
    console.log("Remote info is: ", info);
  });

  udpPort.open();

  const app = express();
  const server = app.listen(9091);
  const wss = new ws.Server({
    server: server
  });

  wss.on("connection", function(socket) {
    console.log("connection initialized!");
    const socketPort = new osc.WebSocketPort({
      socket: socket
    });

    var relay = new osc.Relay(udpPort, socketPort, {
      raw: true
    });
  });
}

runOSC(57121, 9091);
runServer("./src/local.js", "dist-local", "http://localhost:8081/", 8081);
runServer("./src/public.js", "dist-public", "http://localhost:8082/", 8082);



