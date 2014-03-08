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

OIFS=$IFS
while read line; do
    IFS=/
    set -- $line
    printf "ETERM *alcove_%s(alcove_state_t *, ETERM *);\n" $1
done < $PROTO

cat<< 'EOF'
char **alcove_list_to_argv(ETERM *);
void alcove_free_argv(char **);

#define ALCOVE_IS_IOLIST(_t)  (ERL_IS_BINARY(_t) || ERL_IS_LIST(_t))
EOF