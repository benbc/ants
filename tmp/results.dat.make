this := $(lastword $(MAKEFILE_LIST))

tmp/results.dat: tmp/ants.beam tmp/core.beam tmp/utils.beam tmp/processes.beam tmp/regulators.beam src/ants.opts $(this)
	. src/ants.opts; \
	erl -pa tmp -noshell -s ants run -s init stop >$@
