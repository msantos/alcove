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
 * execvp(3)
 *
 */
    ETERM *
alcove_execvp(alcove_state_t *ap, ETERM *arg)
{
    ETERM *hd = NULL;
    char *progname = NULL;
    char **argv = NULL;
    int errnum = 0;

    /* progname */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ALCOVE_IS_IOLIST(hd))
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        progname = erl_iolist_to_string(hd);

    if (!progname)
        goto BADARG;

    /* argv */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_LIST(hd))
        goto BADARG;

    if (!ERL_IS_EMPTY_LIST(hd)) {
        argv = alcove_list_to_argv(hd);
        if (!argv)
            goto BADARG;
    }

    execvp(progname, argv);

    errnum = errno;

    erl_free(progname);
    alcove_free_argv(argv);

    return alcove_errno(errnum);

BADARG:
    erl_free(progname);
    alcove_free_argv(argv);
    return erl_mk_atom("badarg");
}

/*
 * execve(2)
 *
 */
    ETERM *
alcove_execve(alcove_state_t *ap, ETERM *arg)
{
    ETERM *hd = NULL;
    char *filename = NULL;
    char **argv = NULL;
    char **envp = NULL;
    int errnum = 0;

    /* filename */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ALCOVE_IS_IOLIST(hd))
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        filename = erl_iolist_to_string(hd);

    if (!filename)
        goto BADARG;

    /* argv */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_LIST(hd))
        goto BADARG;

    if (!ERL_IS_EMPTY_LIST(hd)) {
        argv = alcove_list_to_argv(hd);
        if (!argv)
            goto BADARG;
    }

    /* envp */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_LIST(hd))
        goto BADARG;

    if (!ERL_IS_EMPTY_LIST(hd)) {
        envp = alcove_list_to_argv(hd);
        if (!envp)
            goto BADARG;
    }

    execve(filename, argv, envp);

    errnum = errno;

    erl_free(filename);
    alcove_free_argv(argv);
    alcove_free_argv(envp);

    return alcove_errno(errnum);

BADARG:
    erl_free(filename);
    alcove_free_argv(argv);
    alcove_free_argv(envp);
    return erl_mk_atom("badarg");
}
