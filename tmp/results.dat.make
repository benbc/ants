this := $(lastword $(MAKEFILE_LIST))

tmp/results.dat: tmp/ants.beam src/ants.opts $(this)
	. src/ants.opts; \
	erl -pa tmp -noshell -s ants run -s init stop >$@
