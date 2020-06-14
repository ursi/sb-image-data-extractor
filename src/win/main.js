'use strict';

try {
	require(`elm-debug-transformer`).register();
} catch {}

const
	fs = require(`fs`),
	path = require(`path`),
	{ipcRenderer} = require(`electron`),
	jim = require(`@ursi/jim`);

require(`jim-node`)(jim);

const app = Elm.Main.init({flags: __dirname});
