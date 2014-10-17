#!/bin/sh

cat<< EOF
/* Copyright (c) $(date +%Y), Michael Santos <michael.santos@gmail.com>
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */
EOF

PROTO=$1

cat<< 'EOF'

/* calls */
typedef struct {
    ssize_t (*fp)(alcove_state_t *, const char *, size_t, char *, size_t);
    u_int8_t narg;
} alcove_call_t;

const alcove_call_t calls[] = {
EOF

while read line; do
    IFS=/
    set -- $line
    printf "    {alcove_%s, %s},\n" $1 $2
done < $PROTO

cat<< 'EOF'
};
EOF
