"use strict";

const path = require("path");
const express = require("express");
const webpack = require("webpack");
const generateConfig = require("./generate-webpack.config");

const dev  = require("webpack-dev-middleware");
const hot = require("webpack-hot-middleware");



function runServer(entryPath, outputPath, publicPath, portNum) {
  const app = express();
  const config = generateConfig(entryPath, outputPath, publicPath, false);

  const compiler = webpack(config);

  app.use(dev(compiler, {
    noInfo: true,
    publicPath: config.output.publicPath
  }));

  app.use(hot(compiler));

  app.use(express.static(outputPath));
  // app.get("/", function(req, res) {
  //   res.sendFile(path.join(__dirname, outputPath + "/index.html"));
  // });

  app.listen(portNum, "localhost", function (err) {
    if(err) {
      console.log(err);
      return;
    }
  });
}

runServer("./src/local.js", "dist-local", "http://localhost:8081", 8081);
runServer("./src/public.js", "dist-public", "http://localhost:8082", 8082);
