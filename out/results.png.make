this := $(lastword $(MAKEFILE_LIST))

out/results.png: src/ants.plt tmp/results.dat build/gnuplot-spacer $(this)
	cat src/ants.plt tmp/results.dat build/gnuplot-spacer tmp/results.dat | gnuplot >$@
