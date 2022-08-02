#!/usr/bin/env bash

# TODO: run with valgrind once gfortran memory leaks have been fixed
# i.e.  --runner "valgrind --leak-check=full --error-exitcode=1"

set -e

compiler="${1:-gfortran}"

fpm test --compiler "${compiler}" --target unit_test
fpm test --compiler "${compiler}" --target round_trip
./run_io_tests.sh "${1}"
