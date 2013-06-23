out/results.png: src/ants.plt src/e tmp/results.dat Makefile | out
	cat src/ants.plt tmp/results.dat src/e tmp/results.dat | gnuplot >out/results.png

tmp/results.dat: tmp/ants.beam src/ants.opts Makefile | tmp
	. ./src/ants.opts; \
	erl -pa tmp -noshell -s ants run -s init stop > tmp/results.dat

tmp/ants.beam: src/ants.erl Makefile | tmp
	erlc -o tmp src/ants.erl

tmp:
	mkdir -p tmp

out:
	mkdir -p out

.PHONY: clean
clean: Makefile
	rm -rf out tmp
