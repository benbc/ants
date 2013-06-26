.PHONY: clean
clean:
	find out tmp -type f -not -name "*.make" | xargs --no-run-if-empty rm
