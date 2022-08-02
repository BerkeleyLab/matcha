#!/usr/bin/env bash

# TODO: run with valgrind once gfortran memory leaks have been fixed
# i.e.  --runner "valgrind --leak-check=full --error-exitcode=1"

set -e

compiler="${1:-gfortran}"

fpm build --compiler "${compiler}"

simple_put_response=$(fpm test --compiler "${compiler}" --target simple_put | tail -1)
expected_simple_put_response="hello from simple_put"
if [[ "${simple_put_response}" != "${expected_simple_put_response}" ]]; then
    echo "expected '${expected_simple_put_response}' but got '${simple_put_response}'"
    exit 1
fi

string_put_response=$(fpm test --compiler "${compiler}" --target string_put | tail -1)
expected_string_put_response="hello from string_put"
if [[ "${string_put_response}" != "${expected_string_put_response}" ]]; then
    echo "expected '${expected_string_put_response}' but got '${string_put_response}'"
    exit 1
fi

fpm test --compiler "${compiler}" --target simple_get << EOF
hello simple_get
EOF

fpm test --compiler "${compiler}" --target get_maxlen << EOF
hello get_maxlen
EOF

fpm test --compiler "${compiler}" --target get_iostat << EOF
hello get_iostat
EOF

fpm test --compiler "${compiler}" --target get_terminator << EOF
hello,get terminator
EOF

simple_put_line_response=$(fpm test --compiler "${compiler}" --target simple_put_line | tail -1)
expected_simple_put_line_response="hello from simple_put_line"
if [[ "${simple_put_line_response}" != "${expected_simple_put_line_response}" ]]; then
    echo "expected '${expected_simple_put_line_response}' but got '${simple_put_line_response}'"
    exit 1
fi

string_put_line_response=$(fpm test --compiler "${compiler}" --target string_put_line | tail -1)
expected_string_put_line_response="hello from string_put_line"
if [[ "${string_put_line_response}" != "${expected_string_put_line_response}" ]]; then
    echo "expected '${expected_string_put_line_response}' but got '${string_put_line_response}'"
    exit 1
fi
