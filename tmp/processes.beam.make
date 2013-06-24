this := $(lastword $(MAKEFILE_LIST))

tmp/processes.beam: src/processes.erl build/erl-compile $(this)
	build/erl-compile src/processes.erl >$@
