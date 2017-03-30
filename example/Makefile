.PHONY: public/bundle.js
public/bundle.js:
	pulp browserify --main Client -t $@

run-example: public/bundle.js
	pulp -w run --main Server
