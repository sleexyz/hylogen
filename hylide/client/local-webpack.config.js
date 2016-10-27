var generateConfig = require("./generate-webpack.config");

module.exports = generateConfig("./src/local.js", "dist-local", "http://localhost:5678", true);
