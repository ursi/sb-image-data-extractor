'use strict';

try {
	require(`elm-debug-transformer`).register();
} catch {}

const
	fs = require(`fs`),
	path = require(`path`),
	{ipcRenderer} = require(`electron`),
	jim = require(`@ursi/jim`),
	SupPort = require(`@ursi/support`).default;

require(`jim-node`)(jim);

const app = Elm.Main.init({flags: __dirname});

const port = SupPort(app.ports);

port(`ports`, {
	ToJson(value) {
		console.log(value);
		fs.writeFileSync(path.join(__dirname, `image-data.json`), JSON.stringify(value));
		console.log("file written");
	},

	LogValue: console.log
}, {
	// DataReceived(send) {
	// 	send(fs.readFileSync(path.join(__dirname, `image-data.txt`), `utf-8`));
	// },
	// JsonReceived(send) {
	// 	send(JSON.parse(fs.readFileSync(path.join(__dirname, `image-data.json`), `utf-8`)));
	// }
});
