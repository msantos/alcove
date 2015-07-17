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
#include "alcove_call.h"

/* Probably only useful for testing */
    ssize_t
alcove_sys_alloc(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;
    int rindex = 0;
    char buf[MAXMSGLEN] = {0};
    size_t size = sizeof(buf);
    alcove_alloc_t *elem = NULL;
    ssize_t nelem = 0;

    if (alcove_decode_list_to_buf(arg, len, &index, buf, &size,
                &elem, &nelem) < 0)
        return -1;

    ALCOVE_TUPLE3(reply, &rindex,
        "ok",
        alcove_encode_binary(reply, rlen, &rindex, buf, size),
        alcove_encode_buf_to_list(reply, rlen, &rindex, buf, size, elem, nelem)
    );

    return rindex;
}
