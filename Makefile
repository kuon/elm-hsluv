SHELL := /bin/bash
PATH := ./node_modules/.bin:$(PATH)


.PHONY: test

test:
	elm test


.PHONY: watch

watch:
	elm test --watch

.PHONY: clean

clean:
	rm -fr elm-stuff
	rm -fr tests/elm-stuff
