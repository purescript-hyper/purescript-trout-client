.PHONY: output/bundle.js
output/bundle.js:
	pulp browserify --main Example.Client -I example -t $@

run-example: output/bundle.js
	pulp -w run --main Example.Server -I example
