default: build

SHELL := /usr/bin/env bash

clean:
	rm public/main.js

distclean: clean
	rm -Rf elm-stuff

production:
	elm make --optimize src/Main.elm --output public/main.js

build:
	elm make src/Main.elm --output public/main.js

serve: build
	(cd public && elm reactor)
