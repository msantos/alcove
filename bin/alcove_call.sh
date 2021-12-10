#!/bin/sh

cat<< EOF
/* GENERATED: DO NOT EDIT */
/* clang-format off */
EOF

while IFS=/ read call arity; do
    cat << EOF
ssize_t alcove_sys_${call}(alcove_state_t *, const char *, size_t, char *, size_t);
EOF
done < "$1"
