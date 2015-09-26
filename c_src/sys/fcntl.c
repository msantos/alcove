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

#include <fcntl.h>
#include "alcove_fcntl_constants.h"

typedef struct {
    u_char type;
    int arg;
    char data[MAXMSGLEN];
    size_t len;
} alcove_fcntl_arg_t;

enum {
    ALCOVE_FCNTL_INT,
    ALCOVE_FCNTL_CSTRUCT,
    ALCOVE_FCNTL_BINARY
};

/*
 * fcntl(2)
 *
 */
    ssize_t
alcove_sys_fcntl(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;
    int rindex = 0;
    int type = 0;
    int arity = 0;

    int fd = 0;
    int cmd = 0;
    alcove_fcntl_arg_t argp = {0};

    int rv = 0;

    /* fd */
    if (alcove_decode_int(arg, len, &index, &fd) < 0)
        return -1;

    /* cmd */
    switch (alcove_decode_define(arg, len, &index, &cmd,
                alcove_fcntl_constants)) {
        case 0:
            break;
        case 1:
            return alcove_mk_error(reply, rlen, "unsupported");
        default:
            return -1;
    }

    /* argp */
    if (alcove_get_type(arg, len, &index, &type, &arity) < 0)
        return -1;

    switch (type) {
        case ERL_SMALL_INTEGER_EXT:
        case ERL_INTEGER_EXT:
            argp.type = ALCOVE_FCNTL_INT;
            if (alcove_decode_int(arg, len, &index, &argp.arg) < 0)
                return -1;

            break;

        default:
            return -1;
    }

    rv = fcntl(fd, cmd, argp.arg);

    if (rv < 0)
        return alcove_mk_errno(reply, rlen, errno);

    ALCOVE_OK(
        reply,
        rlen,
        &rindex,
        alcove_encode_long(reply, rlen, &rindex, rv)
    );

    return rindex;
}
