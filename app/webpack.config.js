var webpack = require("webpack");
var path = require("path");

var entry = [
  "webpack-hot-middleware/client",
  "./src/entry.js"
];


var plugins = [
  new webpack.HotModuleReplacementPlugin(),
  new webpack.NoErrorsPlugin()
];


module.exports = {
  output: {
    path: path.join(__dirname, "dist"),
    filename: "bundle.js",
    publicPath: "http://localhost:8080/"
  },

  resolve: {
    extensions: ["", ".js"]
  },
  entry: entry,
  plugins: plugins,
  devtool: "cheap-module-eval-source-map"
};
