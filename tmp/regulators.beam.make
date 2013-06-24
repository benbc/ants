this := $(lastword $(MAKEFILE_LIST))

tmp/regulators.beam: build/erl-compile src/regulators.erl $(this)
	build/erl-compile src/regulators.erl >$@
