.PHONY: example/client-server/public/bundle.js
example/public/bundle.js:
	spago bundle-app --main Client --path=example/client-server/src/*.purs --to=example/client-server/public/bundle.js

client-server-example: example/client-server/public/bundle.js
	spago run --main Server --path=example/client-server/src/*.purs

