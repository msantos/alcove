/* Copyright (c) 2015, Michael Santos <michael.santos@gmail.com>
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
#include "alcove.h"
#include <ctype.h>

static int alcove_encode_atom_to_lower(char *buf, size_t len, int *index,
        const char *p);

    int
alcove_encode_constant_id(char *buf, size_t len, int *index, long long val,
        const alcove_constant_t *constants)
{
    const alcove_constant_t *dp = NULL;

    for (dp = constants; dp->name != NULL; dp++) {
        if (val == dp->val)
            return alcove_encode_atom_to_lower(buf, len, index, dp->name);
    }

    return alcove_encode_atom(buf, len, index, "unknown");
}

    static int
alcove_encode_atom_to_lower(char *buf, size_t len, int *index, const char *p)
{
    char atom[MAXATOMLEN] = {0};
    char *q = atom;

    for ( ; *p; p++, q++)
        *q = tolower((int)(unsigned char)*p);

    return alcove_encode_atom(buf, len, index, atom);
}
