SECP256K1_DIST=$(shell pwd)/secp256k1_dist/

$(shell pkg-config libsecp256k1)
ifeq "$(.SHELLSTATUS)" "0"
$(warning "libsecp256k1 exists")

.PHONY: scep256k1_dist
secp256k1_dist:
else
$(warning "libsecp256k1 does not exist")

PKG_CONFIG_PATH=$(SECP256K1_DIST)/usr/lib/pkgconfig/
export PKG_CONFIG_PATH
secp256k1_dist: secp256k1
	cd secp256k1/ && autoreconf -vfi
	cd secp256k1/ && ./configure \
			--prefix=$(SECP256K1_DIST)/usr/ \
			--sbindir=$(SECP256K1_DIST)/usr/bin \
			--libexecdir=$(SECP256K1_DIST)/usr/bin \
			--sysconfdir=$(SECP256K1_DIST)/etc \
			--sharedstatedir=$(SECP256K1_DIST)/share/libsecp256k1 \
			--localstatedir=$(SECP256K1_DIST)/var/lib/libsecp256k1 \
			--disable-static \
			--disable-benchmark \
			--disable-coverage \
			--enable-module-ecdh \
			--enable-module-recovery \
			--enable-module-extrakeys \
			--enable-module-schnorrsig \
			--enable-tests \
			--enable-exhaustive-tests \
			--with-gnu-ld
	cd secp256k1/ && make -j
	cd secp256k1/ && make check
	cd secp256k1/ && make install

endif

.PHONY: clean
clean:
	stack clean --full
	rm -rf $(SECP256K1_DIST)

.PHONY: build
build: secp256k1_dist
	stack build --test --no-run-tests

.PHONY: test
test:
	stack build --test

.PHONY: ghci
ghci:
	stack ghci

.PHONY: docs
docs:
	stack haddock --open

.PHONY: deps
deps:
	stack build --copy-compiler-tool hlint stylish-haskell

.PHONY: format
format:
	find . -name '*.hs' | xargs -t stack exec -- stylish-haskell -i

.PHONY: lint
lint:
	stack exec -- hlint -i 'Parse error' -i 'Reduce duplication' -i 'Use <=<' src
