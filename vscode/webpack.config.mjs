// @ts-check
import path from "node:path";
import webpack from "webpack";

const __dirname = new URL('.', import.meta.url).pathname;

/** @typedef {import('webpack').Configuration} WebpackConfig **/
/** @type WebpackConfig */
const webExtensionConfig = {
  mode: 'none',
  target: 'webworker',
  entry: {
    index: './src/index.ts',
    // 'test/suite/index': './src/web/test/suite/index.ts'
  },
  output: {
    filename: '[name].js',
    path: path.join(__dirname, './dist'),
    libraryTarget: 'commonjs',
    devtoolModuleFilenameTemplate: '../[resource-path]'
  },
  resolve: {
    mainFields: ['browser', 'module', 'main'],
    extensions: ['.ts', '.js'],
    alias: {},
    fallback: {
      assert: import.meta.resolve('assert')
    }
  },
  module: {
    rules: [
      {
        test: /\.ts$/,
        exclude: /node_modules/,
        use: [
          {
            loader: 'ts-loader'
          }
        ]
      }
    ]
  },
  plugins: [
    new webpack.ProvidePlugin({
      process: import.meta.resolve('process/browser.js')
    })
  ],
  externals: {
    vscode: 'commonjs vscode'
  },
  performance: {
    hints: false
  },
  devtool: 'nosources-source-map'
};
export default [webExtensionConfig];
