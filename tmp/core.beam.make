this := $(lastword $(MAKEFILE_LIST))

tmp/core.beam: build/erl-compile src/core.erl $(this)
	build/erl-compile src/core.erl >$@
