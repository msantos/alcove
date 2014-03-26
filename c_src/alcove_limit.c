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
#include "alcove_limit.h"

/*
 * getrlimit(2)
 *
 */
    ETERM *
alcove_getrlimit(alcove_state_t *ap, ETERM *arg)
{
    ETERM *hd = NULL;
    int resource = 0;
    struct rlimit rlim = {0};
    int rv = 0;

    /* resource */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_INTEGER(hd))
        goto BADARG;

    resource = ERL_INT_VALUE(hd);

    rv = getrlimit(resource, &rlim);

    return ( (rv < 0)
            ? alcove_errno(errno)
            : alcove_ok(alcove_tuple3(
                    erl_mk_atom("rlimit"),
                    erl_mk_ulonglong(rlim.rlim_cur),
                    erl_mk_ulonglong(rlim.rlim_max)
                    )));

BADARG:
    return erl_mk_atom("badarg");
}

/*
 * setrlimit(2)
 *
 */
    ETERM *
alcove_setrlimit(alcove_state_t *ap, ETERM *arg)
{
    ETERM *hd = NULL;
    ETERM *t = NULL;
    int resource = 0;
    int cur = 0, max = 0;
    struct rlimit rlim = {0};
    int rv = 0;

    /* resource */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_INTEGER(hd))
        goto BADARG;

    resource = ERL_INT_VALUE(hd);

    /* XXX rlim_cur/rlim_max may be uint64_t */
    /* {rlimit, rlim_cur, rlim_max} */

    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_TUPLE(hd) || erl_size(hd) != 3)
        goto BADARG;

    /* 'rlimit' */
    t = erl_element(1, hd);
    if (!t || !ERL_IS_ATOM(t) || strncmp(ERL_ATOM_PTR(t), "rlimit", 6))
        goto BADARG;

    /* rlim_cur: soft limit */
    t = erl_element(2, hd);
    if (!t || !ERL_IS_INTEGER(t))
        goto BADARG;

    cur = ERL_INT_UVALUE(t);

    /* rlim_max: hard limit */
    t = erl_element(3, hd);
    if (!t || !ERL_IS_INTEGER(t))
        goto BADARG;

    max = ERL_INT_UVALUE(t);

    rlim.rlim_cur = cur;
    rlim.rlim_max = max;

    rv = setrlimit(resource, &rlim);

    return ( (rv < 0) ? alcove_errno(errno) : erl_mk_atom("ok"));

BADARG:
    return erl_mk_atom("badarg");
}

/*
 * rlimit constants
 *
 */
    ETERM *
alcove_rlimit_define(alcove_state_t *ap, ETERM *arg)
{
    ETERM *hd = NULL;
    char *name = NULL;

    /* name */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_ATOM(hd))
        goto BADARG;

    name = ERL_ATOM_PTR(hd);

    return alcove_define(name, alcove_rlimit_constants);

BADARG:
    return erl_mk_atom("badarg");
}
