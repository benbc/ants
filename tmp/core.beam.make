this := $(lastword $(MAKEFILE_LIST))

tmp/core.beam: src/core.erl build/erl-compile $(this)
	build/erl-compile src/core.erl >$@
