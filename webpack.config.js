const path = require('path');

module.exports = {
  entry: {
    retetris: './lib/js/src/index.js'
  },
  output: {
    path: path.join(__dirname, "bundledOutputs"),
    filename: '[name].js',
  },
};
