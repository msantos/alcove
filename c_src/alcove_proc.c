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

#ifdef __linux__
#include <sys/prctl.h>
#include "alcove_proc.h"
#endif

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
 * prctl(2)
 *
 */
    ETERM *
alcove_prctl(alcove_state_t *ap, ETERM *arg)
{
#ifdef __linux__
    ETERM *hd = NULL;
    int option = 0;

    alcove_prctl_arg_t arg2 = {0};
    alcove_prctl_arg_t arg3 = {0};
    alcove_prctl_arg_t arg4 = {0};
    alcove_prctl_arg_t arg5 = {0};

    int rv = 0;
    ETERM *t[6] = {0};

    /* option */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_INTEGER(hd))
        goto BADARG;

    option = ERL_INT_VALUE(hd);

    /* XXX unsigned long vs unsigned int */

    /* arg2 */
    arg = alcove_list_head(&hd, arg);
    if (!hd)
        goto BADARG;

    PROPT(hd, arg2);

    /* arg3 */
    arg = alcove_list_head(&hd, arg);
    if (!hd)
        goto BADARG;

    PROPT(hd, arg3);

    /* arg4 */
    arg = alcove_list_head(&hd, arg);
    if (!hd)
        goto BADARG;

    PROPT(hd, arg3);

    /* arg5 */
    arg = alcove_list_head(&hd, arg);
    if (!hd)
        goto BADARG;

    PROPT(hd, arg3);

    rv = prctl(option, PRARG(arg2), PRARG(arg3), PRARG(arg4), PRARG(arg5));

    if (rv < 0)
        return alcove_errno(errno);

    t[0] = erl_mk_atom("ok");
    t[1] = erl_mk_int(rv);
    t[2] = PRVAL(arg2);
    t[3] = PRVAL(arg3);
    t[4] = PRVAL(arg4);
    t[5] = PRVAL(arg5);

    PRFREE(arg2);
    PRFREE(arg3);
    PRFREE(arg4);
    PRFREE(arg5);

    return erl_mk_tuple(t, 6);

BADARG:
    PRFREE(arg2);
    PRFREE(arg3);
    PRFREE(arg4);
    PRFREE(arg5);
    return erl_mk_atom("badarg");
#else
    return alcove_error("unsupported");
#endif
}

/*
 * prctl flags
 *
 */
    ETERM *
alcove_prctl_define(alcove_state_t *ap, ETERM *arg)
{
#ifdef __linux__
    ETERM *hd = NULL;
    char *name = NULL;

    /* name */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_ATOM(hd))
        goto BADARG;

    name = ERL_ATOM_PTR(hd);

    return alcove_define(name, alcove_prctl_constants);

BADARG:
    return erl_mk_atom("badarg");
#else
    return erl_mk_atom("false");
#endif
}
