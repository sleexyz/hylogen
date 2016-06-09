var generateConfig = require("./generate-webpack.config");

module.exports = generateConfig("./src/public.js", "dist-public", "https://hylogen.com", true);
