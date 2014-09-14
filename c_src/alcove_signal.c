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
#include "alcove_signal.h"

/*
 * kill(2)
 *
 */
    ssize_t
alcove_kill(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;

    pid_t pid = 0;
    int sig = 0;
    int rv = 0;

    /* pid */
    if (alcove_decode_int(arg, len, &index, &pid) < 0)
        return -1;

    /* signal */
    if (alcove_decode_int(arg, len, &index, &sig) < 0)
        return -1;

    rv = kill(pid, sig);

    return (rv < 0)
        ? alcove_mk_errno(reply, rlen, errno)
        : alcove_mk_atom(reply, rlen, "ok");
}

/*
 * sigaction(2)
 *
 */
    ssize_t
alcove_sigaction(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;
    int type = 0;
    int arity = 0;

    int signum = 0;
    char signame[MAXATOMLEN] = {0};
    char handler[MAXATOMLEN] = {0};
    struct sigaction act = {{0}};
    int rv = 0;

    /* signum */
    if (alcove_get_type(arg, len, &index, &type, &arity) < 0)
        return -1;

    switch (type) {
        case ERL_ATOM_EXT:
            if (alcove_decode_atom(arg, len, &index, signame) < 0)
                return -1;

            if (alcove_lookup_define(signame, (unsigned long long *)&signum,
                        alcove_signal_constants) < 0)
                return -1;

            break;

        case ERL_SMALL_INTEGER_EXT:
        case ERL_INTEGER_EXT:
            if (alcove_decode_int(arg, len, &index, &signum) < 0)
                return -1;
            break;

        default:
            return -1;
    }

    /* handler */
    if (alcove_decode_atom(arg, len, &index, handler) < 0)
        return -1;

    if (!strncmp(handler, "dfl", 3)) {
        act.sa_handler = SIG_DFL;
    }
    else if (!strncmp(handler, "ign", 3)) {
        act.sa_handler = SIG_IGN;
    }
    else if (!strncmp(handler, "trap", 4)) {
        act.sa_handler = sighandler;
    }
    else {
        return -1;
    }

    if (signum == SIGCHLD) {
        ALCOVE_SETOPT(ap, alcove_opt_sigchld,
                ((act.sa_handler == sighandler) ? 1 : 0));
        return alcove_mk_atom(reply, rlen, "ok");
    }

    rv = sigaction(signum, &act, NULL);

    return (rv < 0)
        ? alcove_mk_errno(reply, rlen, errno)
        : alcove_mk_atom(reply, rlen, "ok");
}

/*
 * signals
 *
 */
    ssize_t
alcove_signal_define(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;
    int rindex = 0;

    char name[MAXATOMLEN] = {0};

    /* constant */
    if (alcove_decode_atom(arg, len, &index, name) < 0)
        return -1;

    ALCOVE_ERR(alcove_encode_version(reply, rlen, &rindex));
    ALCOVE_ERR(alcove_encode_define(reply, rlen, &rindex,
                name, alcove_signal_constants));

    return rindex;
}

    ssize_t
alcove_signal_constant(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;
    int rindex = 0;

    int signum = 0;

    /* signum */
    if (alcove_decode_int(arg, len, &index, &signum) < 0)
        return -1;

    ALCOVE_ERR(alcove_encode_version(reply, rlen, &rindex));
    ALCOVE_ERR(alcove_encode_constant(reply, rlen, &rindex,
                signum, alcove_signal_constants));

    return rindex;
}
