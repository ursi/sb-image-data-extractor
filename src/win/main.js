'use strict';

try {
	require(`elm-debug-transformer`).register();
} catch {}

const app = Elm.Main.init();

const
	fs = require(`fs`),
	path = require(`path`),
	{ipcRenderer} = require(`electron`);
	//SupPort = require(`./web-modules/SupPort`).default;

//const port = SupPort(app.ports);


//app.ports.receiveData.send(fs.readFileSync(path.join(__dirname, `image-data.txt`), `utf-8`));

const data = JSON.parse(fs.readFileSync(path.join(__dirname, `image-data.json`)));
app.ports.receiveJson.send(data);

//console.log(data);
/*const images =
	data
		.filter(o => o.head === `sheetDef`)
		.map((o, index) => {
			return o.body
				.filter(o => o.head === `imageDef`)
				.map(o => {
					const image = Object.fromEntries(o.body)
					return {
						  sheet: index
						, x: image.x
						, y: image.y
						, w: image.w
						, h: image.h
						, objects: new Set
						}
				});
		})
		.flat();

for (const o of data) {
	if (o.head === `animatedImageDesc`) {
		let vid;
		for (const o2 of o.body) {
			if (o2[0] === `vid`)
				vid = o2[1].replace(/"/g, ``);
			else if (o2.head === `animDesc`)
				for (const o3 of o2.body)
					if (o3.head === `frameDesc`)
						for (const o4 of o3.body)
							if (o4.head === `element`) {
								const element = Object.fromEntries(o4.body);
								images[element.uiid].objects.add(vid);
							}
		}
	}
}*/

//console.log(images);

app.ports.toJson.subscribe(value => {
	console.log(value);
	fs.writeFileSync(path.join(__dirname, `image-data.json`), JSON.stringify(value));
	console.log("file written");
});


app.ports.logValue.subscribe(console.log);
