/* Copyright (c) 2014-2017, Michael Santos <michael.santos@gmail.com>
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

    int
alcove_decode_constant(const char *buf, size_t len, int *index, int *val,
        const alcove_constant_t *constants)
{
    int type = 0;
    int arity = 0;

    char define[MAXATOMLEN] = {0};

    long long constant = 0;

    if (alcove_get_type(buf, len, index, &type, &arity) < 0)
        return -1;

    switch (type) {
        case ERL_ATOM_EXT:
            if (alcove_decode_atom(buf, len, index, define) < 0)
                return -1;

            if (alcove_lookup_constant(define, &constant, constants) < 0)
                return 1;

            if (constant < INT32_MIN || constant > INT32_MAX)
                return -1;

            *val = constant;

            break;

        case ERL_SMALL_INTEGER_EXT:
        case ERL_INTEGER_EXT:
            if (alcove_decode_int(buf, len, index, val) < 0)
                return -1;
            break;

        default:
            return -1;
    }

    return 0;
}
