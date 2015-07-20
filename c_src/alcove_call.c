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
#include "alcove_calls.h"

#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <err.h>
#include <errno.h>

#include <sys/types.h>

static int count_pid(alcove_state_t *ap, alcove_child_t *c,
        void *arg1, void *arg2);
static int cons_pid(alcove_state_t *ap, alcove_child_t *c,
        void *arg1, void *arg2);

    ssize_t
alcove_call(alcove_state_t *ap, u_int32_t call,
        const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;
    int arity = 0;
    int version = 0;

    const alcove_call_t *fun = NULL;
    ssize_t written = 0;

    if (call >= sizeof(calls)/sizeof(calls[0]))
        goto BADARG;

    fun = &calls[call];

    if ( (len <= 2) || ((ei_decode_version(arg, &index, &version) < 0)
            && version != ERL_VERSION_MAGIC))
        goto BADARG;

    if (alcove_decode_tuple_header(arg, len, &index, &arity) < 0)
        goto BADARG;

    if (arity != fun->narg)
        goto BADARG;

    written = (*fun->fp)(ap, arg+index, len-index, reply, rlen);

    if (written < 0)
        goto BADARG;

    return written;

BADARG:
    return alcove_mk_atom(reply, rlen, "badarg");
}

    ssize_t
alcove_sys_version(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    return alcove_mk_binary(reply, rlen,
            ALCOVE_VERSION, sizeof(ALCOVE_VERSION)-1);
}

    ssize_t
alcove_sys_pid(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int rindex = 0;
    size_t count = 0;

    /* Count the number of PIDs */
    if (pid_foreach(ap, 0, &count, NULL, pid_not_equal, count_pid) < 0)
        return -1;

    /* Ensure the buffer is large enough to hold the PIDs
     * version = 1 byte
     *     list header = 5 bytes
     *     tuple header = 5 bytes
     *     atom = 1 byte
     *     alcove_pid = 10 bytes
     *     4 * int (7) = 28 bytes
     * tail = 1 byte
     *
     * length = 1 + (count * 49) + 1
     */
    if (2 + count * 49 > rlen)
        return -1;

    if (ei_encode_version(reply, &rindex) < 0)
        return -1;

    if (pid_foreach(ap, 0, reply, &rindex, pid_not_equal, cons_pid) < 0)
        return -1;

    if (ei_encode_empty_list(reply, &rindex) < 0)
        return -1;

    return rindex;
}

/* Get port options */
    ssize_t
alcove_sys_getopt(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;
    char opt[MAXATOMLEN] = {0};
    int val = -1;

    /* opt */
    if (alcove_decode_atom(arg, len, &index, opt) < 0)
        return -1;

    if (strcmp(opt, "verbose") == 0) {
        val = ap->verbose;
    }
    else if (strcmp(opt, "childlimit") == 0) {
        val = ap->fdsetsize;
    }
    else if (strcmp(opt, "exit_status") == 0) {
        val = ap->opt & alcove_opt_exit_status ? 1 : 0;
    }
    else if (strcmp(opt, "maxchild") == 0) {
        val = ap->maxchild;
    }
    else if (strcmp(opt, "maxforkdepth") == 0) {
        val = ap->maxforkdepth;
    }
    else if (strcmp(opt, "termsig") == 0) {
        val = ap->opt & alcove_opt_termsig ? 1 : 0;
    }
    else if (strcmp(opt, "stdin_closed") == 0) {
        val = ap->opt & alcove_opt_stdin_closed ? 1 : 0;
    }
    else if (strcmp(opt, "stdout_closed") == 0) {
        val = ap->opt & alcove_opt_stdout_closed ? 1 : 0;
    }
    else if (strcmp(opt, "stderr_closed") == 0) {
        val = ap->opt & alcove_opt_stderr_closed ? 1 : 0;
    }

    return (val == -1)
        ? alcove_mk_atom(reply, rlen, "false")
        : alcove_mk_ulong(reply, rlen, val);
}

/* Set port options */
    ssize_t
alcove_sys_setopt(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;
    char opt[MAXATOMLEN] = {0};
    u_int32_t val = 0;

    /* opt */
    if (alcove_decode_atom(arg, len, &index, opt) < 0)
        return -1;

    /* val */
    if (alcove_decode_uint(arg, len, &index, &val) < 0)
        return -1;

    if (strcmp(opt, "verbose") == 0) {
        ap->verbose = val;
    }
    else if (strcmp(opt, "exit_status") == 0) {
        ALCOVE_SETOPT(ap, alcove_opt_exit_status, val);
    }
    else if (strcmp(opt, "maxchild") == 0) {
        ap->maxchild = val;
    }
    else if (strcmp(opt, "maxforkdepth") == 0) {
        ap->maxforkdepth = val;
    }
    else if (strcmp(opt, "termsig") == 0) {
        ALCOVE_SETOPT(ap, alcove_opt_termsig, val);
    }
    else if (strcmp(opt, "stdin_closed") == 0) {
        ALCOVE_SETOPT(ap, alcove_opt_stdin_closed, val);
    }
    else if (strcmp(opt, "stdout_closed") == 0) {
        ALCOVE_SETOPT(ap, alcove_opt_stdout_closed, val);
    }
    else if (strcmp(opt, "stderr_closed") == 0) {
        ALCOVE_SETOPT(ap, alcove_opt_stderr_closed, val);
    }
    else
        return alcove_mk_atom(reply, rlen, "false");

    return alcove_mk_atom(reply, rlen, "true");
}

/* Convert an errno integer to an atom */
    ssize_t
alcove_sys_errno_id(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;
    int errnum = 0;

    /* errno */
    if (alcove_decode_int(arg, len, &index, &errnum) < 0)
        return -1;

    return alcove_mk_atom(reply, rlen, erl_errno_id(errnum));
}

/*
 * Utility functions
 */
    static int
count_pid(alcove_state_t *ap, alcove_child_t *c, void *arg1, void *arg2)
{
    size_t *n = arg1;
    *n += 1;
    return 1;
}

    static int
cons_pid(alcove_state_t *ap, alcove_child_t *c, void *arg1, void *arg2)
{
    char *buf = arg1;
    int *index = arg2;

    if (ei_encode_list_header(buf, index, 1) < 0)
        return -1;

    if (ei_encode_tuple_header(buf, index, 5) < 0)
        return -1;

    if (ei_encode_atom(buf, index, "alcove_pid") < 0)
        return -1;

    if (ei_encode_long(buf, index, c->pid) < 0)
        return -1;

    if (ei_encode_long(buf, index, c->fdin) < 0)
        return -1;

    if (ei_encode_long(buf, index, c->fdout) < 0)
        return -1;

    if (ei_encode_long(buf, index, c->fderr) < 0)
        return -1;

    return 1;
}
