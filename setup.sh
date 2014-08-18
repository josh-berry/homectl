#!/bin/bash

homectl="$(dirname "$0")/homectl.hcpkg/bin/hc"

if [[ "$1" == -* ]] || [[ -z "$1" ]]; then
    exec "$homectl" help init
fi

exec "$homectl" init "$@"
