this := $(lastword $(MAKEFILE_LIST))

tmp/monitor.beam: build/erl-compile src/monitor.erl $(this)
	build/erl-compile src/monitor.erl >$@
