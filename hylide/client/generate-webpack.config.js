var webpack = require("webpack");
var path = require("path");

module.exports = function (entryPath, outputPath, publicPath, isProd) {
  var entry =  isProd
        ? [
          entryPath,
        ]
        : [
          "webpack-hot-middleware/client?reload=true",
          entryPath
        ];


  var plugins = isProd
        ? [
          new webpack.optimize.DedupePlugin(),
          new webpack.NoErrorsPlugin()
        ]
        : [
          new webpack.HotModuleReplacementPlugin(),
          new webpack.NoErrorsPlugin()
        ];

  return {
    output: {
      path: path.join(__dirname, outputPath),
      filename: "bundle.js",
      publicPath: publicPath
    },
    module: {
      loaders: [
        {
          test: /\.jsx?$/,
          exclude: /(node_modules|bower_components)/,
          loader: 'babel-loader',
        }
      ]
    },
    resolve: {
      extensions: [".js", ".jsx"]
    },
    externals: {
      osc: "osc"
    },
    entry: entry,
    plugins: plugins,
    devtool: isProd ? false : "cheap-module-eval-source-map"
  };
}


