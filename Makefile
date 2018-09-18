SHELL := /bin/bash
PATH := ./node_modules/.bin:$(PATH)


.PHONY: test

test:
	elm-test tests/Tests.elm


.PHONY: watch

watch:
	elm-test --watch

.PHONY: clean

clean:
	rm -fr elm-stuff
	rm -fr tests/elm-stuff
