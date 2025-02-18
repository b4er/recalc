#!/usr/bin/env bash
# make sure the version specified in the (cabal|vscode extension) config
# match with the ones in their respective changelog

if [ "$1" == "cabal" ]; then
  prefix="."
  version="$(grep '^version:' recalc.cabal | awk -F: '{print $2}' | xargs)"
  ts_version="$(grep '^version:' recalc-vscode/recalc-ts-defs.cabal | awk -F: '{print $2}' | xargs)"
  if [ "${version}" != "${ts_version}" ]; then
    echo "cabal versions for recalc and recalc-ts-defs mismatch (${version} vs. ${ts_version})" >&2
    exit 2
  fi
else
  prefix="recalc-vscode"
  #version="$(jq -r .version < "$prefix/package.json")"
  version="$(grep '^[ ]*"version"[ ]*:' recalc-vscode/package.json | tr -cd '0-9.')"
fi

if ! grep -q "^## ${version} -- [0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]$" "${prefix}/CHANGELOG.md"; then
  echo "version mismatch (${1})" >&2
  exit 1
fi
