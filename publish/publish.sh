#!/usr/bin/env bash

set -euxo pipefail

find ../../WurstStdlib2/ -name '*.wurst' | \
    head -n 100 | \
    xargs -I {} bash -c " \
        mkdir -p \$( echo '{}' | cut -d'/' -f5- | rev | cut -d'/' -f2- | rev ) && \
        ../target/release/wurstdoktor.exe < {} > \$( echo '{}' | cut -d '/' -f5- ).yaml \
    "
