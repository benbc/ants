this := $(lastword $(MAKEFILE_LIST))

tmp/results.dat: src/monitor src/run $(this)
	envsubst <src/run \
	| sed -e 's/^/trapping timeout --preserve-status 20s /' \
	| parallel --ungroup
