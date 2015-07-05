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
#include "alcove_fork.h"

/*
 * fork(2)
 *
 */
    ssize_t
alcove_sys_fork(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int rindex = 0;

    alcove_arg_t child_arg = {0};
    alcove_stdio_t fd = {{0}};
    pid_t pid = 0;
    sigset_t oldset;
    sigset_t set;
    int errnum = 0;

    if (ap->depth >= ap->maxforkdepth)
        return alcove_mk_errno(reply, rlen, EAGAIN);

    if (pid_foreach(ap, 0, NULL, NULL, pid_equal, avail_pid) != 0)
        return alcove_mk_errno(reply, rlen, EAGAIN);

    if (alcove_stdio(&fd) < 0)
        return alcove_mk_errno(reply, rlen, errno);

    (void)sigfillset(&set);
    (void)sigemptyset(&oldset);

    if (sigprocmask(SIG_BLOCK, &set, &oldset) < 0)
        return alcove_mk_errno(reply, rlen, errno);

    child_arg.ap = ap;
    child_arg.fd = &fd;
    child_arg.sigset = &oldset;

    pid = fork();

    switch (pid) {
        case -1:
            errnum = errno;
            if (sigprocmask(SIG_SETMASK, &oldset, NULL) < 0)
                exit(errno);
            return alcove_mk_errno(reply, rlen, errnum);
        case 0:
            if (alcove_child_fun(&child_arg) < 0)
                exit(errno);
            exit(0);
        default:
            if (sigprocmask(SIG_SETMASK, &oldset, NULL) < 0)
                return alcove_mk_errno(reply, rlen, errno);

            if (alcove_parent_fd(ap, &fd, pid) < 0)
                return alcove_mk_errno(reply, rlen, errno);

            ALCOVE_OK(reply, &rindex,
                alcove_encode_long(reply, rlen, &rindex, pid));

            return rindex;
    }
}
