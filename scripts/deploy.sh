#!/bin/bash
set -o nounset -o pipefail -o errexit

rsync -v dist/* freddy:jaspervdj.be/beeraffe/

echo 2>&1 "https://jaspervdj.be/beeraffe/"
