#!/bin/sh
[ -d "${HOME}/bin" ] && PATH="${HOME}/bin:${PATH}"
[ -d "${HOME}/.local/bin" ] && PATH="${HOME}/.local/bin:${PATH}"
[ -d "${HOME}/.node_modules/bin" ] && PATH="${HOME}/.node_modules/bin:${PATH}"
[ -d "${HOME}/.cabal/bin" ] && PATH="${HOME}/.cabal/bin:${PATH}"

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

export PATH
export TERMINAL=termite
export ALTERNATE_EDITOR=
export EDITOR=emacsclient
