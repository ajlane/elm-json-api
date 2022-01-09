'use strict';

const path = require('path')
const TerserPlugin = require("terser-webpack-plugin");
const ShebangPlugin = require('webpack-shebang-plugin');

module.exports = {
    target: 'node',
    mode: 'production',
    bail: true,
    entry: './main.js',
    output: {
        path: path.resolve(__dirname, 'target'),
        filename: 'seed-api.js'
    },
    optimization: {
        minimize: true,
        minimizer: [
            new TerserPlugin({
                terserOptions: {
                    ecma: 8,
                    compress: {
                        passes: 2,
                        warnings: false,
                        pure_getters: true,
                        keep_fargs: false,
                        unsafe_comps: true,
                        unsafe: true,
                        pure_funcs: [
                            'A2',
                            'A3',
                            'A4',
                            'A5',
                            'A6',
                            'A7',
                            'A8',
                            'A9',
                            'F2',
                            'F3',
                            'F4',
                            'F5',
                            'F6',
                            'F7',
                            'F8',
                            'F9',
                        ],
                    }
                }
            })
        ]
    },
    resolve: {
        modules: [path.resolve(__dirname, 'src'), 'node_modules'],
        extensions: ['.js', '.mjs', '.elm'],
    },
    module: {
        strictExportPresence: true,
        rules: [
            {
                test: /\.elm$/,
                exclude: [/[/\\\\]elm-stuff[/\\\\]/, /[/\\\\]node_modules[/\\\\]/],
                use: [
                    {
                        loader: require.resolve('elm-webpack-loader'),
                        options: {
                            optimize: true
                        },
                    },
                ],
            }
        ],
    },
    plugins: [
        new ShebangPlugin()
    ]
};