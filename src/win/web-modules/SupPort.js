export default portsObj => {
	return (portBaseName, handler, inOnly = {}, useOutOnly = false) => {
		if (typeof portBaseName !== `string`)
			typeError(portBaseName, `1st`, `a string`, portBaseName);
		if (!isObject(handler))
			typeError(portBaseName, `2nd`, `an object`, handler);
		if (!isObject(inOnly))
			typeError(portBaseName, `3rd`, `an object or undefined`, inOnly);
		if (typeof useOutOnly !== `boolean`)
			typeError(portBaseName, `4th`, `a boolean or 'undefined'`, useOutOnly);
		if (useOutOnly === true && !isEmpty(inOnly))
			throw `${errorHeader(portBaseName)}

You're using an out-only port but the 3rd argument is not an empty object.
To use an out-only port, this object must be empty.

If you do not wish to use an out-only port, remove the 4th argument ('true').`;

		const
			inPortName = portBaseName + `In`,
			outPortName = portBaseName + `Out`,
			inPort = portsObj[inPortName],
			outPort = portsObj[outPortName];

		let send;
		if (inPort === undefined) {
			if (useOutOnly === false)
				noPortError(portsObj, inPortName, `in`);
			else
				send = msg => {
					throw `${errorHeader(portBaseName, msg)}

You tried to send some data back into Elm, but this port is set up as "out only".

This means that the 4th argument you passed into the port handling function was 'true'.
Try removing that.`
				}
		} else {
			const portSend = portsObj[inPortName].send;
			send = (msg, data) => portSend({msg, data});
		}

		if (outPort === undefined) {
			if (!isEmpty(handler))
				noPortError(portsObj, outPortName, `out`);
		} else {
			outPort.subscribe(async ({msg, data}) => {
				const func = handler[msg];

				if (func === undefined) {
					throw `${errorHeader(portBaseName)}

'${msg}' is not a valid message name.`;
				} else {
					const in_ = await func.call(handler, data);

					if (Array.isArray(in_)) {
						const [inMsg, inDataOrFunction] = in_
						if (typeof inDataOrFunction === `function`)
							inDataOrFunction(inData => send(inMsg, inData), send);
						else
							send(inMsg, inDataOrFunction);
					} else if (typeof in_ === `string`) {
						send(in_);
					} else if (in_ !== undefined) {
						throw `${errorHeader(portBaseName, msg)}

You tried to return ${in_}.

The only values that can be returned are an array, string, function, or undefined, or a promise that resolves to one of those.`;
					}
				}
			});
		}

		Object.entries(inOnly).map(([msg, sendFunction]) => {
			try {
				sendFunction(data => send(msg, data), send);
			} catch (error) {
				if (error.name === `TypeError`)
					throw `${errorHeader(portBaseName)}

The values of the in-only object must be functions.`;
				else
					throw error;
			}
		});
	}
};

function errorHeader(...args) {
	return `SupPort Error: ` + args.join(` -> `);
}

function typeError(portBaseName, position, allowed, passed) {
	throw `${errorHeader(portBaseName)}

The ${position} argument was ${passed}:
This argument must be ${allowed}.`
}

function noPortError(portsObj, portName, portType) {
	throw `${errorHeader(`There is no "${portName}" port`)}

To use SupPort, ports have to use the naming convention "portName(In|Out)".
Out-ports are for sending values out of Elm.
In-ports are for sending values into Elm.

${noPortErrorHelper(portsObj, portType)}`
}

function noPortErrorHelper(portsObj, portType) {
	if (portType === `in`)
		return `${noPortErrorHelperHelper(`out`, `'true'`, `4th`)}

Your in-ports are:
${createInPortsString(portsObj)}`;
	else
		return `${noPortErrorHelperHelper(`in`, `an empty object`, `2nd`)}

Your out-ports are:
${createOutPortsString(portsObj)}`;
}

function noPortErrorHelperHelper(a, b, c) {
	return `To use an ${a}-port by itself, pass ${b} as the ${c} argument to your port handling function.`
}

function createInPortsString(portsObj) {
	return createPortsStringHelper(
		Object.entries(portsObj)
			.filter(([_, value]) => value.hasOwnProperty(`send`))
	);
}

function createOutPortsString(portsObj) {
	return createPortsStringHelper(
		Object.entries(portsObj)
			.filter(([_, value]) => value.hasOwnProperty(`subscribe`))
	);
}

function createPortsStringHelper(list) {
	return list
		.map(([k, _]) => `\t` + k)
		.sort()
		.join(`\n`);
}

function isEmpty(obj) {
	return Object.keys(obj).length === 0;
}

function isObject(maybeObj) {
	return typeof maybeObj === `object` && maybeObj !== null;
}
