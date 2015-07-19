.PHONY: run clean test tour

SRC := $(wildcard api/src/*.hs) \
       $(wildcard core/*.hs)
OBJ := $(SRC:%.hs=%) $(SRC:%.hs=%.hi) $(SRC:%.hs=%.o)
SNAP :=api/dist/build/snap/snap

run: $(SNAP)
	(cd api; ./dist/build/snap/snap;)


$(SNAP): $(SRC) api/snap.cabal
	(cd api; cabal configure; cabal build;)

clean:
	rm -rf api/dist $(OBJ)

test:
	(cd core; ghc --make test;)

tour:
	(cd core; ghc --make test-tour;)
