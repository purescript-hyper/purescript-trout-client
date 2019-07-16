.PHONY: example/public/bundle.js
example/public/bundle.js:
	spago bundle-app -p example/src/**/*.purs --main Client

run-example: example/public/bundle.js
	spago run -w --main Server -p example/src/**/*.purs
