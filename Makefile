.PHONY: all
all:
	$(MAKE) -C lib/github.com/diku-dk/sml-http all

.PHONY: test
test:
	$(MAKE) -C lib/github.com/diku-dk/sml-http test

.PHONY: clean
clean:
	$(MAKE) -C lib/github.com/diku-dk/sml-http clean
	rm -rf MLB *~ .*~
