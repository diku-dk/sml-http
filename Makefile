SMLPKG ?= smlpkg

.PHONY: all
all: lib/github.com/diku-dk/sml-parse
	$(MAKE) -C lib/github.com/diku-dk/sml-http all

.PHONY: test
test: lib/github.com/diku-dk/sml-parse
	$(MAKE) -C lib/github.com/diku-dk/sml-http test

.PHONY: clean
clean:
	$(MAKE) -C lib/github.com/diku-dk/sml-http clean
	find . -name 'MLB' | xargs rm -rf
	rm -rf MLB *~ .*~

.PHONY: realclean
realclean:
	$(MAKE) clean
	rm -rf lib/github.com/diku-dk/sml-parse

lib/github.com/diku-dk/sml-parse:
	$(SMLPKG) sync
