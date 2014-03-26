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

    return (rv < 0 ? alcove_errno(errno) : erl_mk_atom("ok"));

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

    return (rv < 0 ? alcove_errno(errno) : erl_mk_atom("ok"));

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
signum_to_atom(int signum)
{
    switch (signum) {
#ifdef SIGHUP
        case SIGHUP:
            return erl_mk_atom("hup");
#endif
#ifdef SIGINT
        case SIGINT:
            return erl_mk_atom("int");
#endif
#ifdef SIGQUIT
        case SIGQUIT:
            return erl_mk_atom("quit");
#endif
#ifdef SIGILL
        case SIGILL:
            return erl_mk_atom("ill");
#endif
#ifdef SIGTRAP
        case SIGTRAP:
            return erl_mk_atom("trap");
#endif
#ifdef SIGABRT
        case SIGABRT:
            return erl_mk_atom("abrt");
#endif
#ifdef SIGBUS
        case SIGBUS:
            return erl_mk_atom("bus");
#endif
#ifdef SIGFPE
        case SIGFPE:
            return erl_mk_atom("fpe");
#endif
#ifdef SIGKILL
        case SIGKILL:
            return erl_mk_atom("kill");
#endif
#ifdef SIGUSR1
        case SIGUSR1:
            return erl_mk_atom("usr1");
#endif
#ifdef SIGSEGV
        case SIGSEGV:
            return erl_mk_atom("segv");
#endif
#ifdef SIGUSR2
        case SIGUSR2:
            return erl_mk_atom("usr2");
#endif
#ifdef SIGPIPE
        case SIGPIPE:
            return erl_mk_atom("pipe");
#endif
#ifdef SIGALRM
        case SIGALRM:
            return erl_mk_atom("alrm");
#endif
#ifdef SIGTERM
        case SIGTERM:
            return erl_mk_atom("term");
#endif
#ifdef SIGSTKFLT
        case SIGSTKFLT:
            return erl_mk_atom("stkflt");
#endif
#ifdef SIGCHLD
        case SIGCHLD:
            return erl_mk_atom("chld");
#endif
#ifdef SIGCONT
        case SIGCONT:
            return erl_mk_atom("cont");
#endif
#ifdef SIGSTOP
        case SIGSTOP:
            return erl_mk_atom("stop");
#endif
#ifdef SIGTSTP
        case SIGTSTP:
            return erl_mk_atom("tstp");
#endif
#ifdef SIGTTIN
        case SIGTTIN:
            return erl_mk_atom("ttin");
#endif
#ifdef SIGTTOU
        case SIGTTOU:
            return erl_mk_atom("ttou");
#endif
#ifdef SIGURG
        case SIGURG:
            return erl_mk_atom("urg");
#endif
#ifdef SIGXCPU
        case SIGXCPU:
            return erl_mk_atom("xcpu");
#endif
#ifdef SIGXFSZ
        case SIGXFSZ:
            return erl_mk_atom("xfsz");
#endif
#ifdef SIGVTALRM
        case SIGVTALRM:
            return erl_mk_atom("vtalrm");
#endif
#ifdef SIGPROF
        case SIGPROF:
            return erl_mk_atom("prof");
#endif
#ifdef SIGWINCH
        case SIGWINCH:
            return erl_mk_atom("winch");
#endif
#ifdef SIGIO
        case SIGIO:
            return erl_mk_atom("io");
#endif
#ifdef SIGLOST
        case SIGLOST:
            return erl_mk_atom("lost");
#endif
#ifdef SIGPWR
        case SIGPWR:
            return erl_mk_atom("pwr");
#endif
#ifdef SIGSYS
        case SIGSYS:
            return erl_mk_atom("sys");
#endif
#ifdef SIGSTKSZ
        case SIGSTKSZ:
            return erl_mk_atom("stksz");
#endif
        default:
            return alcove_tuple2(
                    erl_mk_atom("unknown"),
                    erl_mk_int(signum)
                    );
    }
}
