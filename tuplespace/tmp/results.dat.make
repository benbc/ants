this := $(lastword $(MAKEFILE_LIST))

tmp/results.dat: src/monitor src/run src/loader src/worker $(this)
	envsubst <src/run \
	| sed -e 's/^/trapping timeout 20s /' \
	| parallel --jobs 0 --ungroup
