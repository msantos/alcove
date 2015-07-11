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

/*
 * read(2)
 *
 */
    ssize_t
alcove_sys_read(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;
    int rindex = 0;

    int fd = -1;
    size_t count = 0;
    char buf[MAXMSGLEN] = {0};
    size_t maxlen = 0;
    int rv = 0;

    /* fd */
    if (alcove_decode_int(arg, len, &index, &fd) < 0)
        return -1;

    /* count */
    if (alcove_decode_ulonglong(arg, len, &index,
                (unsigned long long *)&count) < 0)
        return -1;

    /* Silently truncate too large values of count */
    maxlen = ALCOVE_MSGLEN(ap->depth, sizeof(buf));

    rv = read(fd, buf, MIN(count,maxlen));

    if (rv < 0)
        return alcove_mk_errno(reply, rlen, errno);

    ALCOVE_OK(reply, &rindex,
        alcove_encode_binary(reply, rlen, &rindex, buf, rv));

    return rindex;
}
