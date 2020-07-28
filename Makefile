.DEFAULT_GOAL := examples

.PHONY: example/client-server/public/bundle.js
example/client-server/public/bundle.js:
	spago bundle-app --main Example.ClientServer.Client --path=example/client-server/src/*.purs --to=example/client-server/public/bundle.js

client-server-example: example/client-server/public/bundle.js
	spago run --main Example.ClientServer.Server --path=example/client-server/src/*.purs

.PHONY: example/client/public/bundle.js
example/client/public/bundle.js:
	spago bundle-app --main Example.Client.Main --path=example/client/src/*.purs --to=example/client/public/bundle.js

client-example: example/client/public/bundle.js

examples: client-example example/client-server/public/bundle.js
