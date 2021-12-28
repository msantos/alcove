#!/bin/sh

cat <<EOF
/* GENERATED: DO NOT EDIT */
/* clang-format off */
EOF

nr=0

while read -r _; do
	nr=$((nr + 1))
done <"$1"

cat <<EOF
#define ALCOVE_MAX_NR   ${nr}
#define ALCOVE_NR_SIZE  ((${nr} / 8) + (${nr} % 8))
EOF
