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
#include "alcove_socket_constants.h"

#include <sys/socket.h>

/*
 * socket(2)
 *
 */
    ssize_t
alcove_sys_socket(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;
    int rindex = 0;

    int domain = 0;
    int type = 0;
    int protocol = 0;

    int fd = 0;

    UNUSED(ap);

    /* domain */
    switch (alcove_decode_constant(arg, len, &index, &domain,
                alcove_socket_constants)) {
        case 0:
            break;
        case 1:
            return alcove_mk_error(reply, rlen, "enotsup");
        default:
            return -1;
    }

    /* type */
    switch (alcove_decode_constant(arg, len, &index, &type,
                alcove_socket_constants)) {
        case 0:
            break;
        case 1:
            return alcove_mk_error(reply, rlen, "enotsup");
        default:
            return -1;
    }

    /* protocol */
    if (alcove_decode_int(arg, len, &index, &protocol) < 0)
        return -1;

    fd = socket(domain, type, protocol);

    if (fd < 0)
        return alcove_mk_errno(reply, rlen, errno);

    ALCOVE_OK(
        reply,
        rlen,
        &rindex,
        alcove_encode_long(reply, rlen, &rindex, fd)
    );

    return rindex;
}
