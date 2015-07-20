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
#include "alcove_clone_constants.h"

/*
 * clone(2)
 *
 */
    ssize_t
alcove_sys_clone(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
#ifdef __linux__
    int index = 0;
    int rindex = 0;

    alcove_arg_t child_arg = {0};
    alcove_stdio_t fd = {{0}};
    struct rlimit stack_size = {0};
    char *child_stack = NULL;
    int flags = 0;
    pid_t pid = 0;
    int errnum = 0;
    sigset_t oldset;
    sigset_t set;

    if (ap->depth >= ap->maxforkdepth)
        return alcove_mk_errno(reply, rlen, EAGAIN);

    if (pid_foreach(ap, 0, NULL, NULL, pid_equal, avail_pid) != 0)
        return alcove_mk_errno(reply, rlen, EAGAIN);

    /* flags */
    switch (alcove_decode_define_list(arg, len, &index, &flags,
                alcove_clone_constants)) {
        case 0:
            break;
        case 1:
            return alcove_mk_error(reply, rlen, "unsupported");
        default:
            return -1;
    }

    if (getrlimit(RLIMIT_STACK, &stack_size) < 0)
        return alcove_mk_errno(reply, rlen, errno);

    child_stack = calloc(stack_size.rlim_cur, 1);
    if (child_stack == NULL)
        return alcove_mk_errno(reply, rlen, errno);

    if (alcove_stdio(&fd) < 0)
        goto ERROR;

    (void)sigfillset(&set);
    (void)sigemptyset(&oldset);

    if (sigprocmask(SIG_BLOCK, &set, &oldset) < 0)
        goto ERROR;

    child_arg.ap = ap;
    child_arg.fd = &fd;
    child_arg.sigset = &oldset;

    pid = clone(alcove_child_fun, child_stack + stack_size.rlim_cur,
            flags | SIGCHLD, &child_arg);

    if (sigprocmask(SIG_SETMASK, &oldset, NULL) < 0)
        goto ERROR;

    if (pid < 0)
        goto ERROR;

    free(child_stack);

    if (alcove_parent_fd(ap, &fd, pid) < 0)
        return alcove_mk_errno(reply, rlen, errno);

    ALCOVE_OK(reply, &rindex,
        alcove_encode_long(reply, rlen, &rindex, pid)
    );

    return rindex;

ERROR:
    errnum = errno;
    free(child_stack);
    return alcove_mk_errno(reply, rlen, errnum);
#else
    return alcove_mk_atom(reply, rlen, "undef");
#endif
}
