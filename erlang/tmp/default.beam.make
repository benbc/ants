this := $(lastword $(MAKEFILE_LIST))

tmp/%.beam: src/%.erl build/erl-compile $(this)
	build/erl-compile $< >$@
