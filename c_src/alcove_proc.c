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

/*
 * getpid(2)
 *
 */
    ETERM *
alcove_getpid(alcove_state_t *ap, ETERM *arg)
{
    return erl_mk_int(getpid());
}

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
