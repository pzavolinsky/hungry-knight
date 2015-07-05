Hungry Knight
=============

Hungry Knight is an extremely simplified version of the [Knight's Tour](https://en.wikipedia.org/wiki/Knight%27s_tour) problem. Essentially you are a Knight and must capture every pawn in the board.

This project is intended as an excuse to build a real app using Haskell that, incidentally, might be fun to play with.

Installation
------------

To build this app you'll need [GHC](https://www.haskell.org/ghc/) and [Snap](http://snapframework.com/download).

Just as a hint, here are some shell commands randomly collected from my `.bash_history` (note that some of these might not even run):

```Shell
sudo apt-get install ghc
sudo apt-get install libghc-random-dev
sudo apt-get install cabal-install
cabal update
cabal install snap
```

Then, cross your fingers and run:

```Shell
make
```

Finally browse [http://localhost:8000/](http://localhost:8000/).
