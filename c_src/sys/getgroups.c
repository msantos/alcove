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
#include "alcove_call.h"

/*
 * getgroups(2)
 *
 */
    ssize_t
alcove_sys_getgroups(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int rindex = 0;
    int size = 0;
    int n = 0;
    gid_t *list = NULL;
    int errnum = 0;

    UNUSED(ap);
    UNUSED(arg);
    UNUSED(len);

    size = getgroups(0, NULL);
    if (size < 0)
        return alcove_mk_errno(reply, rlen, errno);

    list = calloc(size, sizeof(gid_t));
    if (list == NULL)
        return alcove_mk_errno(reply, rlen, errno);

    n = getgroups(size, list);

    if (n < 0) {
        errnum = errno;
        free(list);
        return alcove_mk_errno(reply, rlen, errnum);
    }

    ALCOVE_ERR(alcove_encode_version(reply, rlen, &rindex));
    ALCOVE_ERR(alcove_encode_tuple_header(reply, rlen, &rindex, 2));
    ALCOVE_ERR(alcove_encode_atom(reply, rlen, &rindex, "ok"));

    n--;
    for ( ; n >= 0; n--) {
        ALCOVE_ERR(alcove_encode_list_header(reply, rlen, &rindex, 1));
        ALCOVE_ERR(alcove_encode_long(reply, rlen, &rindex, list[n]));
    }

    ALCOVE_ERR(alcove_encode_empty_list(reply, rlen, &rindex));

    free(list);

    return rindex;
}
