.PHONY: example/public/bundle.js
example/public/bundle.js:
	spago bundle-app --main Client --path=example/src/*.purs --to=example/public/bundle.js

run-example: example/public/bundle.js
	spago run --main Server --path=example/src/*.purs
