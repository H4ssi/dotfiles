#!/bin/sh
[ -d "${HOME}/bin" ] && export PATH="${HOME}/bin:${PATH}"
[ -d "${HOME}/node_modules/.bin" ] && export PATH="${HOME}/node_modules/.bin:${PATH}"
[ -d "${HOME}/.cabal/bin" ] && export PATH="${HOME}/.cabal/bin:${PATH}"

setup_gem_path() {
    if which ruby gem > /dev/null; then
        local IFS=:
        for p in $(gem environment gempath); do
            export PATH="${PATH}:$p/bin"
        done
    fi
}

setup_gem_path
unset -f setup_gem_path
