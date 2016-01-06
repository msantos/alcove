/* Copyright (c) 2016, Michael Santos <michael.santos@gmail.com>
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

#if defined(__FreeBSD__)
#include <sys/capsicum.h>
#include "alcove_cap_constants.h"
#endif

/*
 * cap_fcntls_limit(2)
 *
 */
    ssize_t
alcove_sys_cap_fcntls_limit(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
#if defined(__FreeBSD__)
    int index = 0;
    int rv = 0;

    int fd = -1;
    union {
        uint32_t u;
        int32_t i;
    } rights = {0};

    UNUSED(ap);

    /* fd */
    if (alcove_decode_int(arg, len, &index, &fd) < 0)
        return -1;

    /* rights */
    switch (alcove_decode_constant_list(arg, len, &index, &rights.i,
                alcove_cap_constants)) {
        case 0:
            break;
        case 1:
            return alcove_mk_error(reply, rlen, "enotsup");
        default:
            return -1;
    }


    rv = cap_fcntls_limit(fd, rights.u);

    return (rv < 0)
        ? alcove_mk_errno(reply, rlen, errno)
        : alcove_mk_atom(reply, rlen, "ok");
#else
    UNUSED(ap);
    UNUSED(arg);
    UNUSED(len);

    return alcove_mk_atom(reply, rlen, "undef");
#endif
}
