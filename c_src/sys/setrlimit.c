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

#if defined(__linux__) || defined(__sunos__) || defined(__OpenBSD__)
static int rlimit_under_maxfd(long maxfd, unsigned long long fd);
#endif

/*
 * setrlimit(2)
 *
 */
    ssize_t
alcove_sys_setrlimit(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;
    int arity = 0;

    int resource = 0;
    char atom[MAXATOMLEN] = {0};
    unsigned long long cur = 0, max = 0;
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

    /* {alcove_rlimit, rlim_cur, rlim_max} */
    if (alcove_decode_tuple_header(arg, len, &index, &arity) < 0 || arity != 3)
        return -1;

    /* 'alcove_rlimit' */
    if (alcove_decode_atom(arg, len, &index, atom) < 0 ||
            strcmp(atom, "alcove_rlimit") != 0)
        return -1;

    /* rlim_cur: soft limit */
    if (alcove_decode_ulonglong(arg, len, &index, &cur) < 0)
        return -1;

    /* rlim_max: hard limit */
    if (alcove_decode_ulonglong(arg, len, &index, &max) < 0)
        return -1;

#if defined(__linux__) || defined(__sunos__) || defined(__OpenBSD__)
    if (resource == RLIMIT_NOFILE) {
        if (rlimit_under_maxfd(ap->maxfd, cur) < 0)
            return alcove_mk_errno(reply, rlen, EINVAL);
    }
#endif

    rlim.rlim_cur = cur;
    rlim.rlim_max = max;

    rv = setrlimit(resource, &rlim);

    return (rv < 0)
        ? alcove_mk_errno(reply, rlen, errno)
        : alcove_mk_atom(reply, rlen, "ok");
}

#if defined(__linux__) || defined(__sunos__) || defined(__OpenBSD__)
    static int
rlimit_under_maxfd(long maxfd, unsigned long long fd)
{
    int i;

    for (i = fd+1; i < maxfd; i++) {
        if (fcntl(fd, F_GETFD, 0) >= 0)
            return -1;
    }

    return 0;
}
#endif
