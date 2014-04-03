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
#include "alcove_calls.h"

#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <err.h>
#include <errno.h>

#include <sys/types.h>

static int cons_pid(alcove_child_t *c, void *arg1, void *arg2);

    ETERM *
alcove_call(alcove_state_t *ap, u_int32_t call, ETERM *arg)
{
    alcove_call_t *fun = NULL;

    if (call >= sizeof(calls)/sizeof(calls[0]))
        return erl_mk_atom("badarg");

    fun = &calls[call];

    if (!ERL_IS_LIST(arg) || erl_length(arg) != fun->narg)
        return erl_mk_atom("badarg");

    return (*fun->fp)(ap, arg);
}

    ETERM *
alcove_version(alcove_state_t *ap, ETERM *arg)
{
    return alcove_bin(ALCOVE_VERSION);
}

    ETERM *
alcove_pid(alcove_state_t *ap, ETERM *arg)
{
    ETERM *t = erl_mk_empty_list();
    (void)pid_foreach(ap, 0, &t, NULL, pid_not_equal, cons_pid);
    return t;
}

/* Get port options */
    ETERM *
alcove_getopt(alcove_state_t *ap, ETERM *arg)
{
    ETERM *hd = NULL;
    char *opt = NULL;

    /* opt */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_ATOM(hd))
        goto BADARG;

    opt = ERL_ATOM_PTR(hd);

    if (!strncmp(opt, "verbose", 7)) {
        return erl_mk_uint(ap->verbose);
    }
    else if (!strncmp(opt, "childlimit", 10)) {
        return erl_mk_uint(ap->fdsetsize);
    }
    else if (!strncmp(opt, "exit_status", 11)) {
        return erl_mk_uint(ap->opt & alcove_opt_exit_status ? 1 : 0);
    }
    else if (!strncmp(opt, "maxchild", 8)) {
        return erl_mk_uint(ap->maxchild);
    }
    else if (!strncmp(opt, "maxforkdepth", 12)) {
        return erl_mk_uint(ap->maxforkdepth);
    }
    else if (!strncmp(opt, "termsig", 7)) {
        return erl_mk_uint(ap->opt & alcove_opt_termsig ? 1 : 0);
    }
    else
        goto BADARG;

BADARG:
    return erl_mk_atom("badarg");
}

/* Set port options */
    ETERM *
alcove_setopt(alcove_state_t *ap, ETERM *arg)
{
    ETERM *hd = NULL;
    char *opt = NULL;
    int val = 0;

    /* opt */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_ATOM(hd))
        goto BADARG;

    opt = ERL_ATOM_PTR(hd);

    /* val */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_INTEGER(hd))
        goto BADARG;

    val = ERL_INT_VALUE(hd);

    if (!strncmp(opt, "verbose", 7)) {
        ap->verbose = val;
    }
    else if (!strncmp(opt, "exit_status", 11)) {
        ap->opt = val ?
            ap->opt | alcove_opt_exit_status :
            ap->opt & ~alcove_opt_exit_status;
    }
    else if (!strncmp(opt, "maxchild", 8)) {
        ap->maxchild = (val > MAXCHILD) ? MAXCHILD : val;
    }
    else if (!strncmp(opt, "maxforkdepth", 12)) {
        ap->maxforkdepth = val;
    }
    else if (!strncmp(opt, "termsig", 7)) {
        ap->opt = val ?
            ap->opt | alcove_opt_termsig :
            ap->opt & ~alcove_opt_termsig;
    }
    else
        goto BADARG;

    return erl_mk_atom("ok");

BADARG:
    return erl_mk_atom("badarg");
}

/*
 * Utility functions
 */

    char **
alcove_list_to_argv(ETERM *arg)
{
    ETERM *hd = NULL;
    ssize_t len = 0;
    int i = 0;
    char **argv = NULL;
    long maxarg = sysconf(_SC_ARG_MAX);

    len = erl_length(arg);

    if (len < 0 || len >= maxarg)
        return NULL;

    /* NULL terminate */
    argv = calloc(len + 1, sizeof(char **));

    if (!argv)
        return NULL;

    for (i = 0; i < len; i++) {
        arg = alcove_list_head(&hd, arg);
        if (!hd)
            return NULL;

        argv[i] = erl_iolist_to_string(hd);
        if (!argv[i])
            return NULL;
    }

    return argv;
}

    void
alcove_free_argv(char **argv)
{
    int i = 0;

    if (argv == NULL)
        return;

    for (i = 0; argv[i]; i++)
        free(argv[i]);

    free(argv);
}

    static int
cons_pid(alcove_child_t *c, void *arg1, void *arg2)
{
    ETERM **t = arg1;
    *t = erl_cons(erl_mk_int(c->pid), *t);
    return 1;
}
