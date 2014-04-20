this := $(lastword $(MAKEFILE_LIST))

out/results.png: src/ants.plt tmp/results.dat $(this)
	cat src/ants.plt tmp/results.dat | gnuplot >$@
