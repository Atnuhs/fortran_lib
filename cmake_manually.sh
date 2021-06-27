#bin/bash

cd $(dirname $0)

cmake -B build
cmake --build build
cmake --build build --target test

cat 'build/Testing/Temporary/LastTest.log'