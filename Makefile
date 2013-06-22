results.png: ants.plt e results.dat Makefile
	cat ants.plt results.dat e results.dat | gnuplot >results.png

results.dat: ants.beam ants.opts Makefile
	. ./ants.opts; \
	erl -noshell -s ants run -s init stop > results.dat

ants.beam: ants.erl Makefile
	erlc ants.erl

.PHONY: clean
clean: Makefile
	rm -f *.beam results.*
