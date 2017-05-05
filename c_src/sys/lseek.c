/* Copyright (c) 2014-2017, Michael Santos <michael.santos@gmail.com>
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
 * lseek(2)
 *
 */
    ssize_t
alcove_sys_lseek(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;

    int fd = 0;
    off_t offset = 0;
    int whence = 0;

    long long val = 0;

    UNUSED(ap);

    /* fd */
    if (alcove_decode_int(arg, len, &index, &fd) < 0)
        return -1;

    /* offset */
    if (alcove_decode_longlong(arg, len, &index, &val) < 0)
        return -1;

    offset = val;

    /* whence */
    if (alcove_decode_int(arg, len, &index, &whence) < 0)
        return -1;

    return (lseek(fd, offset, whence) == -1)
        ? alcove_mk_errno(reply, rlen, errno)
        : alcove_mk_atom(reply, rlen, "ok");
}
