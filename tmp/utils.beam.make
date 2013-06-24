this := $(lastword $(MAKEFILE_LIST))

tmp/utils.beam: src/utils.erl build/erl-compile $(this)
	build/erl-compile src/utils.erl >$@
