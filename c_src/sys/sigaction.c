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
#include "alcove_signal_constants.h"

static sighandler_t atom_to_sighandler(int signum, char *handler);
static char *sighandler_to_atom(sig_t handler);

/*
 * sigaction(2)
 *
 */
    ssize_t
alcove_sys_sigaction(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;
    int rindex = 0;

    int signum = 0;
    char handler[MAXATOMLEN] = {0};
    char *ohandler = NULL;
    struct sigaction act;
    struct sigaction oact;

    UNUSED(ap);

    (void)memset(&act, 0, sizeof(act));
    (void)memset(&oact, 0, sizeof(oact));

    /* signum */
    switch (alcove_decode_constant(arg, len, &index, &signum,
                alcove_signal_constants)) {
        case 0:
            break;
        case 1:
            return alcove_mk_error(reply, rlen, "enotsup");
        default:
            return -1;
    }

    /* handler */
    if (alcove_decode_atom(arg, len, &index, handler) < 0)
        return -1;

    act.sa_handler = atom_to_sighandler(signum, handler);

    if (act.sa_handler == SIG_ERR)
        return -1;

    (void)sigfillset(&act.sa_mask);

    if (sigaction(signum, &act, &oact) < 0)
        alcove_mk_errno(reply, rlen, errno);

    ALCOVE_ERR(alcove_encode_version(reply, rlen, &rindex));
    ALCOVE_ERR(alcove_encode_tuple_header(reply, rlen, &rindex, 2));
    ALCOVE_ERR(alcove_encode_atom(reply, rlen, &rindex, "ok"));

    ohandler = sighandler_to_atom(oact.sa_handler);

    /* Unknown signal handler installed: abort with badarg */
    if (ohandler == NULL)
        return -1;

    ALCOVE_ERR(alcove_encode_atom(reply, rlen, &rindex, ohandler));
    return rindex;
}

    static sig_t
atom_to_sighandler(int signum, char *handler)
{
    if (strcmp(handler, "sig_dfl") == 0) {
        return (signum == SIGCHLD) ? alcove_sig_dfl : SIG_DFL;
    }
    else if (strcmp(handler, "sig_ign") == 0) {
        return SIG_IGN;
    }
    else if (strcmp(handler, "sig_catch") == 0) {
        return alcove_sig_catch;
    }

    return SIG_ERR;
}

    static char *
sighandler_to_atom(sig_t handler)
{
    if (handler == SIG_DFL) {
        return "sig_dfl";
    }
    else if (handler == SIG_IGN) {
        return "sig_ign";
    }
    else if (handler == alcove_sig_catch) {
        return "sig_catch";
    }
    else if (handler == alcove_sig_dfl) {
        return "sig_dfl";
    }

    return NULL;
}
