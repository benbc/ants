results: ants.beam
	erl -noshell -s ants run -s init stop > results

ants.beam: ants.erl
	erlc ants.erl

.PHONY: clean
clean:
	rm -f ants.beam results
