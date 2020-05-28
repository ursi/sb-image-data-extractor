const
	fs = require('fs'),
	path = require('path'),
	{ app
	, BrowserWindow
	} = require('electron');

function createWindow() {
	let win = new BrowserWindow(
		{ /*title: ``
		,*/ width: 1900
		, height: 1000
		, webPreferences: { nodeIntegration: true }
		}
	);

	win.loadFile(path.join('win', 'index.html'));
	win.webContents.on('before-input-event', (event, input) => {
		switch (input.type) {
			case 'keyDown':
				switch (input.key) {
					case 'F12':
						win.webContents.toggleDevTools();
						break;
					case 'F5':
						win.webContents.reload();
						break;
				}

				break;
		}
	});

	// allow iframes from anywhere
	win.webContents.session.webRequest.onHeadersReceived((details, callback) => {
		callback({responseHeaders: Object.fromEntries(Object.entries(details.responseHeaders).filter(header => !/x-frame-options/i.test(header[0])))});
	});
}

app.on(`ready`, createWindow);
