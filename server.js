"use strict";

const path = require("path");
const express = require("express");
const webpack = require("webpack");
const generateConfig = require("./generate-webpack.config");

const dev  = require("webpack-dev-middleware");
const hot = require("webpack-hot-middleware");



function runServer(entryPath, outputPath, portNum) {
  const app = express();
  const config = generateConfig(entryPath, outputPath, portNum);

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

runServer("./src/local.js", "dist-local", 8081);
runServer("./src/public.js", "dist-public", 8082);
