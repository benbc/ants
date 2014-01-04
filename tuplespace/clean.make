.PHONY: clean
clean:
	find out tmp -type f -not -name "*.make" -exec rm {} \;
