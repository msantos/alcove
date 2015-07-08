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
 * setenv(3)
 *
 */

    ssize_t
alcove_sys_setenv(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;
    char name[MAXMSGLEN] = {0};
    size_t namelen = sizeof(name)-1;
    char value[MAXMSGLEN] = {0};
    size_t valuelen = sizeof(value)-1;
    int overwrite = 0;
    int rv = 0;

    /* name */
    if (alcove_decode_iolist(arg, len, &index, name, &namelen) < 0 ||
            namelen == 0)
        return -1;

    /* value */
    if (alcove_decode_iolist(arg, len, &index, value, &valuelen) < 0)
        return -1;

    /* overwrite */
    if (alcove_decode_int(arg, len, &index, &overwrite) < 0)
        return -1;

    rv = setenv(name, value, overwrite);

    return (rv < 0)
        ? alcove_mk_errno(reply, rlen, errno)
        : alcove_mk_atom(reply, rlen, "ok");
}
