.DEFAULT_GOAL := chart

chart: out/results.png
out/results.png: src/ants.plt src/e tmp/results.dat Makefile | out
	cat src/ants.plt tmp/results.dat src/e tmp/results.dat | gnuplot >$@

tmp/results.dat: tmp/ants.beam src/ants.opts Makefile | tmp
	. ./src/ants.opts; \
	erl -pa tmp -noshell -s ants run -s init stop >$@

tmp/ants.beam: src/ants.erl build/erl-compile Makefile | tmp
	build/erl-compile src/ants.erl >$@

tmp:
	mkdir -p $@

out:
	mkdir -p $@

.PHONY: clean
clean: Makefile
	rm -rf out tmp
