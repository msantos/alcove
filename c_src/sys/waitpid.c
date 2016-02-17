/* Copyright (c) 2016, Michael Santos <michael.santos@gmail.com>
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
#include "alcove_wait_constants.h"

#include <sys/wait.h>

static int remove_pid(alcove_state_t *ap, alcove_child_t *c,
        void *arg1, void *arg2);

/*
 * waitpid(2)
 *
 */
    ssize_t
alcove_sys_waitpid(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;
    int rindex = 0;

    pid_t pid = 0;
    int status = 0;
    int options = 0;

    pid_t rv = 0;

    UNUSED(ap);

    /* pid */
    if (alcove_decode_int(arg, len, &index, &pid) < 0)
        return -1;

    /* options */
    switch (alcove_decode_constant_list(arg, len, &index, &options,
                alcove_wait_constants)) {
        case 0:
            break;
        case 1:
            return alcove_mk_error(reply, rlen, "enotsup");
        default:
            return -1;
    }

    rv = waitpid(pid, &status, options);

    if (rv < 0)
        return alcove_mk_errno(reply, rlen, errno);

    if (WIFEXITED(status) || WIFSIGNALED(status)) {
        if (pid_foreach(ap, rv, NULL, NULL, pid_equal, remove_pid) < 0)
            return -1;
    }

    ALCOVE_TUPLE3(
        reply,
        rlen,
        &rindex,
        "ok",
        alcove_encode_long(reply, rlen, &rindex, rv),
        alcove_encode_long(reply, rlen, &rindex, status)
    );

    return rindex;
}

    static int
remove_pid(alcove_state_t *ap, alcove_child_t *c, void *arg1, void *arg2)
{
    UNUSED(ap);
    UNUSED(arg1);
    UNUSED(arg2);

    c->pid = 0;
    c->fdctl = -1;
    c->fdin = -1;
    c->fdout = -1;
    c->fderr = -1;

    return 0;
}
