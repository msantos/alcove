/* Copyright (c) 2014, Michael Santos <michael.santos@gmail.com>
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
alcove_get_type(const char *buf, size_t len, const int *index,
        int *type, int *arity)
{
    const char *s = buf + *index;
    int n = *index + 1;

    if (*index < 0 || *index >= MAXMSGLEN || *index >= len)
        return -1;

    *type = get_int8(s);
    s += 1;

    switch (*type) {
        case ERL_SMALL_ATOM_EXT:
        case ERL_SMALL_ATOM_UTF8_EXT:
            *type = ERL_ATOM_EXT;
        case ERL_SMALL_TUPLE_EXT:
            n += 1;
            if (n > len)
                return -1;

            *arity = get_int8(s);
            break;

        case ERL_ATOM_UTF8_EXT:
            *type = ERL_ATOM_EXT;
        case ERL_ATOM_EXT:
        case ERL_STRING_EXT:
            n += 2;
            if (n > len)
                return -1;

            *arity = get_int16(s);
            break;

        case ERL_LARGE_TUPLE_EXT:
        case ERL_LIST_EXT:
        case ERL_BINARY_EXT:
            n += 4;
            if (n > len)
                return -1;

            *arity = get_int32(s);
            break;

        case ERL_SMALL_BIG_EXT:
            n += 1;
            if (n > len)
                return -1;

            *arity = get_int8(s);
            break;

        case ERL_LARGE_BIG_EXT:
            n += 4;
            if (n > len)
                return -1;

            *arity = get_int32(s);
            break;

        case ERL_SMALL_INTEGER_EXT:
            n += 1;
            if (n > len)
                return -1;

            *arity = 0;
            break;

        case ERL_INTEGER_EXT:
            n += 4;
            if (n > len)
                return -1;

            *arity = 0;
            break;

        case ERL_NIL_EXT:
            *arity = 0;
            break;

        default:
            return -1;
    }

    if (*arity < 0)
        return -1;

    return 0;
}
