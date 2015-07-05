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
#ifdef __linux__
#define _GNU_SOURCE
#include <sched.h>
#ifndef HAVE_SETNS
#include <syscall.h>
#endif
#endif

#include "alcove.h"
#include "alcove_call.h"
#include "alcove_fork.h"

#ifdef __linux__
#ifndef HAVE_SETNS
static int setns(int fd, int nstype);
#endif
#endif

/*
 * setns(2)
 *
 */
#ifdef __linux__
#ifndef HAVE_SETNS
    static int
setns(int fd, int nstype)
{
#ifdef __NR_setns
    return syscall(__NR_setns, fd, nstype);
#else
    errno = ENOSYS;
    return -1;
#endif
}
#endif
#endif

    ssize_t
alcove_sys_setns(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
#ifdef __linux__
    int index = 0;

    char path[PATH_MAX] = {0};
    size_t plen = sizeof(path)-1;
    int fd = -1;
    int rv = 0;
    int errnum = 0;

    /* path */
    if (alcove_decode_iolist(arg, len, &index, path, &plen) < 0 ||
            plen == 0)
        return -1;

    fd = open(path, O_RDONLY);
    if (fd < 0)
        return alcove_mk_errno(reply, rlen, errno);

    rv = setns(fd, 0);

    errnum = errno;

    (void)close(fd);

    if (rv < 0)
        return alcove_mk_errno(reply, rlen, errnum);

    return alcove_mk_atom(reply, rlen, "ok");
#else
    return alcove_mk_atom(reply, rlen, "undef");
#endif
}
