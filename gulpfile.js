const
	gulpBabel = require(`gulp-babel`),
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
	`win/web-modules/**`,
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

globs.babel = input + `win/web-modules/**/*.js`;
function babel() {
	return src(globs.babel)
		.pipe(gulpBabel({plugins: [`@babel/plugin-transform-modules-commonjs`]}))
		.pipe(dest(output + `win/web-modules`));
}

const build =
	parallel
		( copy
		, elm
		, babel
		);

async function watchFiles() {
	const cry = {usePolling: true};
	watch(globs.copy, cry, copy);
	watch(input + `win/src/**/*.elm`, cry, elm);
	watch(globs.babel, babel);
}

exports.default = parallel(build, watchFiles);
