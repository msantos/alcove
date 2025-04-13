#!/bin/sh

set -o errexit
set -o nounset

cat <<EOF
/* GENERATED: DO NOT EDIT */
/* clang-format off */
EOF

while IFS=/ read -r call arity; do
	cat <<EOF
ssize_t alcove_sys_${call}(alcove_state_t *, const char *, size_t, char *, size_t); /* ${call}/$((arity + 2)) */
EOF
done <"$1"
