#!/bin/sh

cat <<EOF
/* GENERATED: DO NOT EDIT */
/* clang-format off */
EOF

cat <<'EOF'

/* calls */
typedef struct {
    ssize_t (*fp)(alcove_state_t *, const char *, size_t, char *, size_t);
    u_int8_t narg;
} alcove_call_t;

static const alcove_call_t calls[] = {
EOF

while IFS=/ read -r call arity; do
	printf "    {alcove_sys_%s, %s},\n" "$call" "$arity"
done <"$1"

cat <<'EOF'
};
EOF
