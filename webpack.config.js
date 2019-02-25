const path = require('path');

module.exports = {
  mode: 'production',
  entry: {index: './build/Beeraffe.js'},
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'Beeraffe.bundle.js'
  },
  module: {
    rules: [
      {
        test: /\.css$/,
        use: [
          {loader: 'css-loader'}
        ]
      },
      {
        test: /\.(png|ttf|txt)$/,
        use: [
          {loader: 'file-loader'}
        ]
      }
    ],
  }
};
