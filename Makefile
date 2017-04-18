.PHONY: example/public/bundle.js
example/public/bundle.js:
	pulp browserify -I example/src --main Client -t $@

run-example: example/public/bundle.js
	pulp -w run --main Server -I example/src
