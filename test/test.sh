#!/bin/bash

DOT=
HOM=
RDOT=

prepare () {
    mkdir -p "$DOT/.link-dir"
    mkdir -p "$DOT/.copy-dir.NO_INSTALL"
    mkdir -p "$DOT/.copy-new-dir.NO_INSTALL"
    
    echo a > "$DOT/.link-file"
    echo b > "$DOT/.diff-match-file"
    echo c > "$DOT/.diff-nomatch-file"

    echo d > "$DOT/.copy-dir.NO_INSTALL/.link-file"
    echo e > "$DOT/.copy-new-dir.NO_INSTALL/.link-file"

    mkdir -p "$HOM/.copy-dir"

    echo f > "$HOM/.existing-file"
    cp "$DOT/.diff-match-file" "$HOM/.diff-match-file"
    echo g > "$HOM/.diff-nomatch-file"

    echo h > "$HOM/.copy-dir/.existing-file"

    echo i > "$DOT/.existing-link"
    echo j > "$DOT/.copy-dir.NO_INSTALL/.existing-link"

    ln -s "$RDOT/.existing-link" "$HOM/.existing-link"
    ln -s "$RDOT/.copy-dir.NO_INSTALL/.existing-link" "$HOM/.copy-dir/.existing-link"
}

expect_link () {
    [[ $(readlink "$1") = "$2" ]] || echo "fail: wrong link: $1 expected to be $2"
}

expect_no_link() {
    [[ -e "$1" ]] || echo "fail: not existent: $1"
    [[ -L "$1" ]] && echo "fail: wrong no link: $1"
}

expect_content () {
    [[ $(cat "$1") = "$2" ]] || echo "fail: wrong content: $1 expected to be $2"
}

check () {
    expect_link "$HOM/.link-file" "$RDOT/.link-file"
    expect_content "$HOM/.link-file" "a"
    expect_link "$HOM/.diff-match-file" "$RDOT/.diff-match-file"
    expect_content "$HOM/.diff-match-file" "b"
    expect_no_link "$HOM/.diff-nomatch-file"
    expect_content "$HOM/.diff-nomatch-file" "g"
    expect_link "$HOM/.link-dir" "$RDOT/.link-dir"
    expect_no_link "$HOM/.copy-dir"
    expect_link "$HOM/.copy-dir/.link-file" "../$RDOT/.copy-dir.NO_INSTALL/.link-file"
    expect_content "$HOM/.copy-dir/.link-file" "d"
    expect_no_link "$HOM/.copy-new-dir"
    expect_link "$HOM/.copy-new-dir/.link-file" "../$RDOT/.copy-new-dir.NO_INSTALL/.link-file"
    expect_content "$HOM/.copy-new-dir/.link-file" "e"
    expect_no_link "$HOM/.existing-file"
    expect_content "$HOM/.existing-file" "f"
    expect_no_link "$HOM/.copy-dir/.existing-file"
    expect_content "$HOM/.copy-dir/.existing-file" "h"
}

expect_dir_ident () {
    [[ $(find "$1" -not -name "install" -printf "%P %l " \( \( -xtype f -exec cat {} \; \) -o -printf "\n" \) | sort) = $(find "$2" -not -name "install" -printf "%P %l " \( \( -xtype f -exec cat {} \; \) -o -printf "\n" \) | sort) ]] || echo "fail: dir not identical: $1 $2"
}

DIR=dotfiles-test-workdir
REF=dotfiles-test-refdir

rm -rf "$DIR" "$REF"

RDOT="dotfiles"
DOT="$DIR/$RDOT"
HOM="$DIR"

prepare

RDOT="../dotfiles/.nested.NO_INSTALL"
DOT="$DIR/dotfiles/.nested.NO_INSTALL"
HOM="$DIR/.nested"

prepare

cp -a "$DIR" "$REF"

expect_dir_ident "$DIR" "$REF"

DOT="$DIR/dotfiles"

cp ../install "$DOT"
pushd "$DOT" > /dev/null
./install
./install # TODO needs to be run twice due to bug after mkdir
popd > /dev/null

expect_dir_ident "$DIR/dotfiles" "$REF/dotfiles"

RDOT="dotfiles"
DOT="$DIR/$RDOT"
HOM="$DIR"

check

RDOT="../dotfiles/.nested.NO_INSTALL"
DOT="$DIR/dotfiles/.nested.NO_INSTALL"
HOM="$DIR/.nested"

check

