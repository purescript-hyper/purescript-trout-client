.PHONY: example/public/bundle.js
example/public/bundle.js:
	pulp browserify --main Example.Client -I example -t $@

run-example: example/public/bundle.js
	pulp -w run --main Example.Server -I example
