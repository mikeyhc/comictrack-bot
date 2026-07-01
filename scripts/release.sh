#!/bin/sh

set -euo pipefail

if [ $# -lt 2 ]; then
    echo "usage: $0 {version}" >&2
    exit 1
fi

BRANCH=$(git rev-parse --abbrev-ref HEAD)
if [ "$BRANCH" != "main" ]; then
    echo "Must be on main to release (currently on $BRANCH)" >&2
    exit 1
fi

VERSION="$1"
TAG="v${VERSION}"

sed -i "s/{vsn, \"[^\"]*\"}/{vsn, \"$VERSION\"}/" src/*.app.src
git add src/*.app.src
git commit -m "chore: bump to version ${VERSION}"
git tag "$TAG"
git push
git push origin "$TAG"
