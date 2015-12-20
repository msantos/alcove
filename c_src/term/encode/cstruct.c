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

    int
alcove_encode_cstruct(char *reply, size_t rlen, int *rindex,
        const char *buf, size_t len,
        alcove_alloc_t *ptr, ssize_t nptr)
{
    int i = 0;
    size_t offset = 0;

    if (alcove_encode_list_header(reply, rlen, rindex, nptr) < 0)
        return -1;

    for ( ; i < nptr; i++) {
        if (ptr[i].p) {
            if (offset + sizeof(void *) > len)
                return -1;

            /* Allocated buffer */
            if (alcove_encode_tuple_header(reply, rlen, rindex, 2) < 0)
                return -1;
            if (alcove_encode_atom(reply, rlen, rindex, "ptr") < 0)
                return -1;
            if (alcove_encode_binary(reply, rlen, rindex, ptr[i].p,
                        ptr[i].len) < 0)
                return -1;
            free(ptr[i].p);
            offset += sizeof(void *);
        }
        else {
            if (offset + ptr[i].len > len)
                return -1;

            /* Static binary */
            if (alcove_encode_binary(reply, rlen, rindex, buf+offset, ptr[i].len) < 0)
                return -1;
            offset += ptr[i].len;
        }
    }

    if (alcove_encode_empty_list(reply, rlen, rindex) < 0)
        return -1;

    free(ptr);
    ptr = NULL;

    return 0;
}
