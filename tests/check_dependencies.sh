#!/usr/bin/env bash

source_files=($(find . -regex '.*\.[rR]'))
grep -hE '\b(require|library)\([\.a-zA-Z0-9]*\)' "${source_files[@]}" | \
    sed '/^[[:space:]]*#/d' | \
    sed -E 's/.*\(([\.a-zA-Z0-9]*)\).*/\1/' | \
    sort -uf \
    > DEPENDS
