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

#include <signal.h>
#include "alcove_signal.h"

/*
 * kill(2)
 *
 */
    ETERM *
alcove_kill(alcove_state_t *ap, ETERM *arg)
{
    ETERM *hd = NULL;
    pid_t pid = 0;
    int sig = 0;
    int rv = 0;

    /* pid */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_INTEGER(hd))
        goto BADARG;

    pid = ERL_INT_VALUE(hd);

    /* signal */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_INTEGER(hd))
        goto BADARG;

    sig = ERL_INT_VALUE(hd);

    rv = kill(pid, sig);

    return (rv < 0) ? alcove_errno(errno) : erl_mk_atom("ok");

BADARG:
    return erl_mk_atom("badarg");
}

/*
 * sigaction(2)
 *
 */
    ETERM *
alcove_sigaction(alcove_state_t *ap, ETERM *arg)
{
    ETERM *hd = NULL;
    int signum = 0;
    char *handler = NULL;
    struct sigaction act = {{0}};
    int rv = 0;

    /* signum */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_INTEGER(hd))
        goto BADARG;

    signum = ERL_INT_VALUE(hd);

    if (signum == SIGCHLD)
        return alcove_errno(EINVAL);

    /* handler */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_ATOM(hd))
        goto BADARG;

    handler = ERL_ATOM_PTR(hd);

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
        goto BADARG;
    }

    rv = sigaction(signum, &act, NULL);

    return (rv < 0) ? alcove_errno(errno) : erl_mk_atom("ok");

BADARG:
    return erl_mk_atom("badarg");
}

/*
 * signals
 *
 */
    ETERM *
alcove_signal_define(alcove_state_t *ap, ETERM *arg)
{
    ETERM *hd = NULL;
    char *name = NULL;

    /* constant */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_ATOM(hd))
        goto BADARG;

    name = ERL_ATOM_PTR(hd);

    return alcove_define(name, alcove_signal_constants);

BADARG:
    return erl_mk_atom("badarg");
}

    ETERM *
alcove_signal_constant(alcove_state_t *ap, ETERM *arg)
{
    ETERM *hd = NULL;
    long long signum = 0;

    /* signum */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_INTEGER(hd))
        goto BADARG;

    signum = ERL_INT_VALUE(hd);

    return signum_to_atom(signum);

BADARG:
    return erl_mk_atom("badarg");
}

    ETERM *
signum_to_atom(int signum)
{
    return alcove_constant(signum, alcove_signal_constants);
}
