this := $(lastword $(MAKEFILE_LIST))

out/results.png: src/ants.plt src/e tmp/results.dat $(this)
	cat src/ants.plt tmp/results.dat src/e tmp/results.dat | gnuplot >$@
