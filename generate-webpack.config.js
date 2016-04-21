var webpack = require("webpack");
var path = require("path");

module.exports = function (entryPath, outputPath, publicPath, isProd) {
  var entry =  isProd
        ? [
          entryPath,
        ]
        : [
          "webpack-hot-middleware/client",
          entryPath
        ];


  var plugins = isProd
        ? [
          new webpack.optimize.UglifyJsPlugin({compressor: {warnings: false}}),
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
    devtool: isProd ? null : "cheap-module-eval-source-map"
  };
}


