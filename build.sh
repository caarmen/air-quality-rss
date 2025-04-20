#!/usr/bin/env bash

# Clean option
if [[ "$1" == "clean" ]]; then
    echo "Cleaning the project..."
    rm -rf build/
    exit 0
fi

mkdir -p build
pushd build || exit 1
cmake ..
make
result=$?
popd || exit
exit $result
