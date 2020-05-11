#!/bin/sh

homectl="$(dirname "$0")/homectl.hcpkg/bin/hc"
exec "$homectl" init "$@"
