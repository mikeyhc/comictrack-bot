#!/bin/sh

if [ $# -lt 1 ]; then
    echo "usage: $0 version" >&2
    exit 1
fi

VERSION="$1"

cd $(git rev-parse --show-toplevel)
echo "$VERSION" > VERSION
sed -i '' "s/{vsn, \".*\"}/{vsn, \"${VERSION}\"}/" src/comictrack_bot.app.src
sed -i '' "s/{release, {comictrack_bot, \".*\"},/{release, {comictrack_bot, \"${VERSION}\"},/" rebar.config
