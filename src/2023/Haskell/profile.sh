#! /usr/bin/zsh
stack run $1 --profile -- +RTS -p
ghc-prof-flamegraph Haskell-exe.prof