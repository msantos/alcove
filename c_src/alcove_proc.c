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
#ifdef HAVE_SECCOMP
#include <linux/seccomp.h>
#endif
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
    alcove_alloc_t *elem[4] = {0};
    ssize_t nelem[4] = {0};
    int i = 0;

    alcove_prctl_arg_t prarg[4] = {{0}};

    int rv = 0;
    ETERM *t[6] = {0};

    /* option */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_INTEGER(hd))
        goto BADARG;

    option = ERL_INT_VALUE(hd);

    /* arg2, arg3, arg4, arg5 */
    for (i = 0; i < 4; i++) {
        arg = alcove_list_head(&hd, arg);
        if (!hd)
            goto BADARG;

        PROPT(hd, prarg[i], elem[i], nelem[i]);
    }

    rv = prctl(option, PRARG(prarg[0]), PRARG(prarg[1]),
            PRARG(prarg[2]), PRARG(prarg[3]));

    if (rv < 0)
        return alcove_errno(errno);

    t[0] = erl_mk_atom("ok");
    t[1] = erl_mk_int(rv);

    for (i = 0; i < 4; i++) {
        switch (prarg[i].type) {
            case ALCOVE_PRARG_UNSIGNED_LONG:
                t[i+2] = erl_mk_ulonglong(prarg[i].arg);
                break;
            case ALCOVE_PRARG_CSTRUCT:
                t[i+2] = alcove_buf_to_list(prarg[i].data, prarg[i].len,
                        elem[i], nelem[i]);
                break;
            case ALCOVE_PRARG_BINARY:
                t[i+2] = erl_mk_binary(prarg[i].data, prarg[i].len);
                break;
        }

        PRFREE(prarg[i], elem[i], nelem[i]);
    }

    return erl_mk_tuple(t, 6);

BADARG:
    for (i = 0; i < 4; i++)
        PRFREE(prarg[i], elem[i], nelem[i]);

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

/*
 * getsid(2)
 */
    ETERM *
alcove_getsid(alcove_state_t *ap, ETERM *arg)
{
    ETERM *hd = NULL;
    pid_t pid = 0;
    pid_t rv = 0;

    /* pid */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_INTEGER(hd))
        goto BADARG;

    pid = ERL_INT_VALUE(hd);

    rv = getsid(pid);

    return rv < 0 ? alcove_errno(errno) : alcove_ok(erl_mk_int(rv));

BADARG:
    return erl_mk_atom("badarg");
}

/*
 * setsid(2)
 */
    ETERM *
alcove_setsid(alcove_state_t *ap, ETERM *arg)
{
    pid_t pid = setsid();
    return pid < 0 ? alcove_errno(errno) : alcove_ok(erl_mk_int(pid));
}

/*
 * getpgrp(2)
 */
    ETERM *
alcove_getpgrp(alcove_state_t *ap, ETERM *arg)
{
    return erl_mk_int(getpgrp());
}
