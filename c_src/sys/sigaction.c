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

/*
 * sigaction(2)
 *
 */
    ssize_t
alcove_sys_sigaction(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;

    int signum = 0;
    char handler[MAXATOMLEN] = {0};
    struct sigaction act;
    int rv = 0;

    (void)memset(&act, 0, sizeof(act));

    /* signum */
    switch (alcove_decode_define(arg, len, &index, &signum,
                alcove_signal_constants)) {
        case 0:
            break;
        case 1:
            return alcove_mk_error(reply, rlen, "unsupported");
        default:
            return -1;
    }

    /* handler */
    if (alcove_decode_atom(arg, len, &index, handler) < 0)
        return -1;

    if (strcmp(handler, "sig_dfl") == 0) {
        act.sa_handler = SIG_DFL;
    }
    else if (strcmp(handler, "sig_ign") == 0) {
        act.sa_handler = SIG_IGN;
    }
    else if (strcmp(handler, "sig_catch") == 0) {
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

    (void)sigfillset(&act.sa_mask);

    rv = sigaction(signum, &act, NULL);

    return (rv < 0)
        ? alcove_mk_errno(reply, rlen, errno)
        : alcove_mk_atom(reply, rlen, "ok");
}
