SRC := $(wildcard api/src/*.hs) \
       $(wildcard core/*.hs)
SNAP :=api/dist/build/snap/snap

run: $(SNAP)
	(cd api; ./dist/build/snap/snap;)


$(SNAP): $(SRC) api/snap.cabal
	(cd api; cabal configure; cabal build;)


