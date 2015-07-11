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
 * chown(2)
 *
 */
    ssize_t
alcove_sys_chown(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;

    char path[PATH_MAX] = {0};
    size_t plen = sizeof(path)-1;
    uid_t owner = {0};
    gid_t group = {0};
    int rv = 0;

    /* path */
    if (alcove_decode_iolist(arg, len, &index, path, &plen) < 0 ||
            plen == 0)
        return -1;

    /* owner */
    if (alcove_decode_uint(arg, len, &index, &owner) < 0)
        return -1;

    /* group */
    if (alcove_decode_uint(arg, len, &index, &group) < 0)
        return -1;

    rv = chown(path, owner, group);

    return (rv < 0)
        ? alcove_mk_errno(reply, rlen, errno)
        : alcove_mk_atom(reply, rlen, "ok");
}
