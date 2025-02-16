TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Find all the env variables starting with CI_ to unset them.
# Otherwise, we might run into environment length limitations on Windows.
# (See `xargs --show-limits`.)
VARS_TO_UNSET := $(shell env | grep ^CI_ | egrep -o '^[^=]+')
unexport $(VARS_TO_UNSET)

clean:
	rm -rf setup a.out dist/build/testA
	rm -rf ,tmp dist ,tmp2
	find . -name "*.o" -exec rm -f {} \;
	find . -name "*.hi" -exec rm -f {} \;
	rm -fr install-tmp
	rm -fr install
	rm -f .setup-config .installed-pkg-config
	rm -rf local.db

# We use the global package database as there's no easy way to tell
# ghc-pkg (via Cabal) to use one in ., and the global one at least
# won't affect the installed GHC and is more likely to work

PREFIX := $(abspath install)
$(eval $(call canonicalise,PREFIX))

cabal01:
	$(MAKE) -s --no-print-directory clean
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -o setup Setup.lhs -v0

	'$(GHC_PKG)' init local.db
# We don't make use of -rtsopts in this test, and if it's enabled then
# we get a warning if dynlibs are enabled by default that:
# 	Warning: -rtsopts and -with-rtsopts have no effect with -shared.
# so we filter the flag out
	./setup configure -v0 --prefix="$(PREFIX)" --with-compiler='$(TEST_HC)' --ghc-options='$(filter-out -rtsopts,$(TEST_HC_OPTS))' --with-hc-pkg='$(GHC_PKG)' --package-db=local.db $(VANILLA) $(PROF) $(DYN) --libsubdir='$$pkgid'
	./setup build -v0
	./setup copy -v0
	echo install1:
	ls -1 install
	rm -r install
#	install w/ register!
	./setup install -v0
	echo install2:
	ls -1 install
	./setup sdist -v0
	echo dist:
	ls -1 dist
	if [ "$(CLEANUP)" != "" ]; then $(MAKE) -s --no-print-directory clean; fi
