this := $(lastword $(MAKEFILE_LIST))

tmp/results.dat: src/monitor src/run src/loader src/worker src/tuplespace \
			src/lib/easy_tcp.py $(this)
	<src/run sed 's/^/trapping timeout 4s /' \
	| parallel --jobs 0 --ungroup >$@
