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
 * setresuid(2)
 */
    ssize_t
alcove_sys_setresuid(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
#if defined(__sunos__)
    return alcove_mk_atom(reply, rlen, "undef");
#else
    int index = 0;
    uid_t uid = {0};
    uid_t euid = {0};
    uid_t suid = {0};
    int rv = 0;

    /* uid */
    if (alcove_decode_uint(arg, len, &index, &uid) < 0)
        return -1;

    /* euid */
    if (alcove_decode_uint(arg, len, &index, &euid) < 0)
        return -1;

    /* suid */
    if (alcove_decode_uint(arg, len, &index, &suid) < 0)
        return -1;

    rv = setresuid(uid, euid, suid);

    return (rv < 0)
        ? alcove_mk_errno(reply, rlen, errno)
        : alcove_mk_atom(reply, rlen, "ok");
#endif
}
