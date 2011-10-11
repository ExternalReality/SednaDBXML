#!/bin/bash

cabal clean
cabal configure
cabal build
cabal install

