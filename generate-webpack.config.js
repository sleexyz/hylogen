var webpack = require("webpack");
var path = require("path");

module.exports = function (entryPath, outputPath, portNum) {

  var entry = [
    "webpack-hot-middleware/client",
    entryPath  
    // "./src/local.js"
  ];


  var plugins = [
    new webpack.HotModuleReplacementPlugin(),
    new webpack.NoErrorsPlugin()
  ];


  return {
    output: {
      path: path.join(__dirname, outputPath),
      // path: path.join(__dirname, "local/"),
      filename: "bundle.js",
      publicPath: "http://localhost:" + portNum + "/"
      // publicPath: "http://localhost:8081/"
    },
    module: {
      loaders: [
        {
          test: /\.jsx?$/,
          exclude: /(node_modules|bower_components)/,
          loader: 'babel',
          query: {
            presets: ['es2015']
          }
        }
      ]
    },
    resolve: {
      extensions: ["", ".js", ".jsx"]
    },
    entry: entry,
    plugins: plugins,
    devtool: "cheap-module-eval-source-map"
  };
}


