this := $(lastword $(MAKEFILE_LIST))

tmp/ants.beam: build/erl-compile src/ants.erl tmp/core.beam $(this)
	build/erl-compile src/ants.erl >$@
