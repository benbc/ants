this := $(lastword $(MAKEFILE_LIST))

tmp/ants.beam: build/erl-compile src/ants.erl $(this)
	build/erl-compile src/ants.erl >$@
