"use strict";

const path = require("path");
const express = require("express");
const webpack = require("webpack");
const config = require("./webpack.config");

const dev  = require("webpack-dev-middleware");
const hot = require("webpack-hot-middleware");



let app = express();

let compiler = webpack(config);

app.use(dev(compiler, {
  noInfo: true,
  publicPath: config.output.publicPath
}));

app.use(hot(compiler));

app.get("*", function(req, res) {
  res.sendFile(path.join(__dirname, "dist/index.html"));
});

app.listen(8080, "localhost", function (err) {
  if(err) {
    console.log(err);
    return;
  }
});
