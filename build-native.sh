#!/bin/sh

set -euo pipefail
declare -rx ORIGINAL_DIR=`pwd`

function cleanup {
    cd $ORIGINAL_DIR
}

trap cleanup EXIT

CURRENT_DIR=`dirname "$0"`
BUILD_DIR="$CURRENT_DIR/test/cpp/build"
echo $BUILD_DIR
mkdir -p $BUILD_DIR
cd $BUILD_DIR
cmake -DCMAKE_BUILD_TYPE=Debug ../
make
