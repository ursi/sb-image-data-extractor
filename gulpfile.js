const
	gulpElm = require(`gulp-elm`),
	gulpRename = require(`gulp-rename`),
	{
		src,
		dest,
		series,
		parallel,
		watch,
	} = require(`gulp`),
	input = `src/`,
	output = `dist/`,
	globs = {};

globs.copy = [input + `**`, ...[
	`node_modules/**`,
	`**/*.elm`,
	`win/elm-stuff/**`,
	`win/src/**`,
	`win/elm.json`,
	`win/elm-git.json`,
].map(g => `!` + input + g)];

function copy() {
	return src(globs.copy)
		.pipe(dest(output));
}

globs.elm = input + `win/src/Main.elm`;
function elm() {
	return src(globs.elm)
		.pipe(gulpElm({cwd: input + `win`}))
		.pipe(gulpRename(`elm.js`))
		.pipe(dest(output + `win`));
}

const build = parallel(copy , elm);

async function watchFiles() {
	const cry = {usePolling: true};
	watch(globs.copy, cry, copy);
	watch(input + `win/src/**/*.elm`, cry, elm);
}

exports.default = parallel(build, watchFiles);
