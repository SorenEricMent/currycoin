.PHONY: haskell-eval
haskell-eval:
	cd haskell && make test

.PHONY: prolog-eval
prolog-eval:
	cd prolog && make test
