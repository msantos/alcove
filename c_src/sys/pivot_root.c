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

#ifdef __linux__
#include <linux/unistd.h>
#endif

/*
 * pivot_root(2)
 *
 */
    ssize_t
alcove_sys_pivot_root(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
#ifdef __linux__
    int index = 0;
    char new_root[PATH_MAX] = {0};
    char put_old[PATH_MAX] = {0};
    size_t nlen = sizeof(new_root)-1;
    size_t plen = sizeof(put_old)-1;
    int rv = 0;

    /* new_root */
    if (alcove_decode_iolist(arg, len, &index, new_root, &nlen) < 0 ||
            nlen == 0)
        return -1;

    /* put_old */
    if (alcove_decode_iolist(arg, len, &index, put_old, &plen) < 0 ||
            plen == 0)
        return -1;

    rv = syscall(__NR_pivot_root, new_root, put_old);

    return (rv < 0)
        ? alcove_mk_errno(reply, rlen, errno)
        : alcove_mk_atom(reply, rlen, "ok");
#else
    return alcove_mk_atom(reply, rlen, "undef");
#endif
}
