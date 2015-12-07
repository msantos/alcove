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
alcove_decode_constant_list(const char *buf, size_t len, int *index, int *val,
        const alcove_constant_t *constants)
{
    int type = 0;
    int arity = 0;

    if (alcove_get_type(buf, len, index, &type, &arity) < 0)
        return -1;

    switch (type) {
        case ERL_NIL_EXT:
            if (ei_decode_list_header(buf, index, &arity) < 0)
                return -1;
            *val = 0;
            break;

        case ERL_SMALL_INTEGER_EXT:
        case ERL_INTEGER_EXT:
            if (alcove_decode_int(buf, len, index, val) < 0)
                return -1;
            break;

        case ERL_STRING_EXT: {
            char tmp[MAXMSGLEN] = {0};
            char *p = tmp;

            if (arity >= sizeof(tmp))
                return -1;

            if (ei_decode_string(buf, index, tmp) < 0)
                return -1;

            for ( ; *p; p++)
                *val |= *p;

            }
            break;

        case ERL_LIST_EXT: {
            int i = 0;
            int length = 0;
            int constant = 0;
            int rv = 0;

            if (ei_decode_list_header(buf, index, &length) < 0)
                return -1;

            for (i = 0; i < length; i++) {
                rv = alcove_decode_constant(buf, len, index, &constant,
                        constants);

                if (rv != 0)
                    return rv;

                *val |= constant;
            }

            /* [] */
            if (alcove_decode_list_header(buf, len, index, &length) < 0
                    || length != 0)
                return -1;

            }
            break;

        default:
            return -1;
    }

    return 0;
}
