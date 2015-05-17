#!/bin/sh
[ -d "${HOME}/bin" ] && export PATH="${HOME}/bin:${PATH}"
[ -d "${HOME}/node_modules/.bin" ] && export PATH="${HOME}/node_modules/.bin:${PATH}"
[ -d "${HOME}/.cabal/bin" ] && export PATH="${HOME}/.cabal/bin:${PATH}"
