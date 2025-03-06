#!/usr/bin/env bash
# make sure the version specified in the (cabal|vscode extension) config
# match with the ones in their respective changelog

if [ "$1" == "cabal" ]; then
  prefix="."
  version="$(grep '^version:' recalc.cabal | awk -F: '{print $2}' | xargs)"
  versions="$(find recalc-* -name 'recalc-*.cabal' -exec grep '^version:' {} \; | awk -F: '{print $2}' | xargs)"

  for v in ${versions}; do
    if [ "${version}" != "${v}" ]; then
      echo "cabal versions mismatch (${version} vs. ${v}):" >&2
      grep -l "^version:[ ]*${v}" recalc-*/recalc-*.cabal >&2
      exit 2
    fi
  done
else
  prefix="recalc-vscode"
  version="$(jq -r .version < "$prefix/package.json")"
fi

if ! grep -q "^## ${version} -- [0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]$" "${prefix}/CHANGELOG.md"; then
  echo "version mismatch (${1})" >&2
  exit 1
fi
