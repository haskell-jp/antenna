#!/bin/bash

set -eux

cd /work
rm -rfd temp
git clone --depth 1 -b gh-pages "https://${GH_TOKEN}@github.com/haskell-jp/antenna.git" temp
cp sites.yaml temp/sites.yaml
cd temp
antenna sites.yaml
git config user.name "${GIT_NAME}"
git status
git add -A
git diff --quiet && git diff --staged --quiet || git commit -am "[skip ci] Update planet haskell. See https://haskell.jp/antenna/ for new entries!"
git push origin gh-pages
