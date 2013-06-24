this := $(lastword $(MAKEFILE_LIST))

tmp/ants.beam: src/ants.erl build/erl-compile $(this)
	build/erl-compile src/ants.erl >$@
