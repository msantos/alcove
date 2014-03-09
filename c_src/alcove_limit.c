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

    /* XXX u_int64_t */
    return ( (rv < 0)
            ? alcove_errno(errno)
            : alcove_ok(alcove_tuple3(
                    erl_mk_atom("rlimit"),
                    erl_mk_uint(rlim.rlim_cur),
                    erl_mk_uint(rlim.rlim_max)
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

    /* {rlimit, rlim_cur, rlim_max} */

    /* XXX rlim_cur, rlim_max = u_int64_t; cur, max = u_int32_t */

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
    char *flag = NULL;

    /* flag */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_ATOM(hd))
        goto BADARG;

    flag = ERL_ATOM_PTR(hd);

    if      (!strncmp(flag, "cpu", 3))          return erl_mk_int(RLIMIT_CPU);
#ifdef RLIMIT_FSIZE
    else if (!strncmp(flag, "fsize", 5))        return erl_mk_int(RLIMIT_FSIZE);
#endif
#ifdef RLIMIT_DATA
    else if (!strncmp(flag, "data", 4))         return erl_mk_int(RLIMIT_DATA);
#endif
#ifdef RLIMIT_STACK
    else if (!strncmp(flag, "stack", 5))        return erl_mk_int(RLIMIT_STACK);
#endif
#ifdef RLIMIT_CORE
    else if (!strncmp(flag, "core", 4))         return erl_mk_int(RLIMIT_CORE);
#endif
#ifdef RLIMIT_RSS
    else if (!strncmp(flag, "rss", 3))          return erl_mk_int(RLIMIT_RSS);
#endif
#ifdef RLIMIT_NPROC
    else if (!strncmp(flag, "nproc", 5))        return erl_mk_int(RLIMIT_NPROC);
#endif
#ifdef RLIMIT_NOFILE
    else if (!strncmp(flag, "nofile", 6))       return erl_mk_int(RLIMIT_NOFILE);
#endif
#ifdef RLIMIT_MEMLOCK
    else if (!strncmp(flag, "memlock", 7))      return erl_mk_int(RLIMIT_MEMLOCK);
#endif
#ifdef RLIMIT_AS
    else if (!strncmp(flag, "as", 2))           return erl_mk_int(RLIMIT_AS);
#endif
#ifdef RLIMIT_LOCKS
    else if (!strncmp(flag, "locks", 5))        return erl_mk_int(RLIMIT_LOCKS);
#endif
#ifdef RLIMIT_SIGPENDING
    else if (!strncmp(flag, "sigpending", 10))  return erl_mk_int(RLIMIT_SIGPENDING);
#endif
#ifdef RLIMIT_MSGQUEUE
    else if (!strncmp(flag, "msgqueue", 8))     return erl_mk_int(RLIMIT_MSGQUEUE);
#endif
#ifdef RLIMIT_NICE
    else if (!strncmp(flag, "nice", 4))         return erl_mk_int(RLIMIT_NICE);
#endif
#ifdef RLIMIT_RTPRIO
    else if (!strncmp(flag, "rtprio", 6))       return erl_mk_int(RLIMIT_RTPRIO);
#endif
#ifdef RLIMIT_RTTIME
    else if (!strncmp(flag, "rttime", 6))       return erl_mk_int(RLIMIT_RTTIME);
#endif
#ifdef RLIMIT_NLIMITS
    else if (!strncmp(flag, "nlimits", 7))      return erl_mk_int(RLIMIT_NLIMITS);
#endif
    else return erl_mk_atom("false");

BADARG:
    return erl_mk_atom("badarg");
}
