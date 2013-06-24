this := $(lastword $(MAKEFILE_LIST))

tmp/ants.beam: build/erl-compile src/ants.erl tmp/core.beam tmp/utils.beam tmp/processes.beam $(this)
	build/erl-compile src/ants.erl >$@
