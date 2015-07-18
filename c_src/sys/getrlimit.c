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
#include "alcove_rlimit_constants.h"

/*
 * getrlimit(2)
 *
 */
    ssize_t
alcove_sys_getrlimit(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;
    int rindex = 0;

    int resource = 0;
    struct rlimit rlim = {0};
    int rv = 0;

    /* resource */
    switch (alcove_decode_define(arg, len, &index, &resource,
                alcove_rlimit_constants)) {
        case 0:
            break;
        case 1:
            return alcove_mk_error(reply, rlen, "unsupported");
        default:
            return -1;
    }

    rv = getrlimit(resource, &rlim);

    if (rv < 0)
        return  alcove_mk_errno(reply, rlen, errno);

    ALCOVE_ERR(alcove_encode_version(reply, rlen, &rindex));
    ALCOVE_ERR(alcove_encode_tuple_header(reply, rlen, &rindex, 2));
    ALCOVE_ERR(alcove_encode_atom(reply, rlen, &rindex, "ok"));
    ALCOVE_ERR(alcove_encode_tuple_header(reply, rlen, &rindex, 3));
    ALCOVE_ERR(alcove_encode_atom(reply, rlen, &rindex, "alcove_rlimit"));
    ALCOVE_ERR(alcove_encode_ulonglong(reply, rlen, &rindex, rlim.rlim_cur));
    ALCOVE_ERR(alcove_encode_ulonglong(reply, rlen, &rindex, rlim.rlim_max));

    return rindex;
}
