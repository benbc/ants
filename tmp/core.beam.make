this := $(lastword $(MAKEFILE_LIST))

tmp/core.beam: build/erl-compile src/core.erl tmp/utils.beam $(this)
	build/erl-compile src/core.erl >$@
