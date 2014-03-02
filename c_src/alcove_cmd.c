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

#define _GNU_SOURCE
#include <fcntl.h>
#include <sched.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <err.h>
#include <errno.h>

#include <sys/types.h>

#include "alcove.h"
#include "alcove_cmd.h"

static char **alcove_list_to_argv(ETERM *);
static void alcove_free_argv(char **);

#define ALCOVE_IS_IOLIST(_t)  (ERL_IS_BINARY(_t) || ERL_IS_LIST(_t))

    ETERM *
alcove_cmd(u_int32_t cmd, ETERM *arg)
{
    alcove_cmd_t *fun = NULL;

    if (cmd >= sizeof(cmds)/sizeof(cmds[0]))
        return erl_mk_atom("badarg");

    fun = &cmds[cmd];

    if (!ERL_IS_LIST(arg) || erl_length(arg) != fun->narg)
        return erl_mk_atom("badarg");

    return (*fun->fp)(arg);
}

    static ETERM *
alcove_version(ETERM *arg)
{
    return alcove_bin(ALCOVE_VERSION);
}

/*
 * chdir(2)
 *
 */
    static ETERM *
alcove_chdir(ETERM *arg)
{
    ETERM *hd = NULL;
    char *path = NULL;
    int rv = 0;

    /* path */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ALCOVE_IS_IOLIST(hd))
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        path = erl_iolist_to_string(hd);

    if (!path)
        goto BADARG;

    rv = chdir(path);

    erl_free(path);

    return ( (rv < 0) ? alcove_errno(errno) : erl_mk_atom("ok"));

BADARG:
    erl_free(path);
    return erl_mk_atom("badarg");
}

/*
 * chroot(2)
 *
 */
    static ETERM *
alcove_chroot(ETERM *arg)
{
    ETERM *hd = NULL;
    char *path = NULL;
    int rv = 0;

    /* path */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ALCOVE_IS_IOLIST(hd))
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        path = erl_iolist_to_string(hd);

    if (!path)
        goto BADARG;

    rv = chroot(path);

    erl_free(path);

    return ( (rv < 0) ? alcove_errno(errno) : erl_mk_atom("ok"));

BADARG:
    erl_free(path);
    return erl_mk_atom("badarg");
}

/*
 * execvp(3)
 *
 */
    static ETERM *
alcove_execvp(ETERM *arg)
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
 * getcwd(3)
 *
 */
    static ETERM *
alcove_getcwd(ETERM *arg)
{
    char buf[PATH_MAX] = {0};

    if (!getcwd(buf, sizeof(buf)))
        return alcove_errno(errno);

    return alcove_ok(erl_mk_binary(buf, strlen(buf)));
}

/*
 * getgid(2)
 *
 */
    static ETERM *
alcove_getgid(ETERM *arg)
{
    return erl_mk_uint(getgid());
}

/*
 * getuid(2)
 *
 */
    static ETERM *
alcove_getuid(ETERM *arg)
{
    return erl_mk_uint(getuid());
}

/*
 * setgid(2)
 *
 */
    static ETERM *
alcove_setgid(ETERM *arg)
{
    ETERM *hd = NULL;
    gid_t gid = {0};
    int rv = 0;

    /* uid */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_INTEGER(hd))
        goto BADARG;

    gid = ERL_INT_UVALUE(hd);

    rv = setgid(gid);

    return ( (rv < 0) ? alcove_errno(errno) : erl_mk_atom("ok"));

BADARG:
    return erl_mk_atom("badarg");
}

/*
 * setuid(2)
 *
 */
    static ETERM *
alcove_setuid(ETERM *arg)
{
    ETERM *hd = NULL;
    uid_t uid = {0};
    int rv = 0;

    /* uid */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_INTEGER(hd))
        goto BADARG;

    uid = ERL_INT_UVALUE(hd);

    rv = setuid(uid);

    return ( (rv < 0) ? alcove_errno(errno) : erl_mk_atom("ok"));

BADARG:
    return erl_mk_atom("badarg");
}

/*
 * getrlimit(2)
 *
 */
    static ETERM *
alcove_getrlimit(ETERM *arg)
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
    static ETERM *
alcove_setrlimit(ETERM *arg)
{
    ETERM *hd = NULL;
    int resource = 0;
    int cur = 0, max = 0;
    struct rlimit rlim = {0};
    int rv = 0;

    /* resource */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_INTEGER(hd))
        goto BADARG;

    resource = ERL_INT_VALUE(hd);

    /* XXX u_int64_t */

    /* rlim_cur: soft limit */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_INTEGER(hd))
        goto BADARG;

    cur = ERL_INT_UVALUE(hd);

    /* rlim_max: hard limit */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_INTEGER(hd))
        goto BADARG;

    max = ERL_INT_UVALUE(hd);

    rlim.rlim_cur = cur;
    rlim.rlim_max = max;

    rv = setrlimit(resource, &rlim);

    return ( (rv < 0) ? alcove_errno(errno) : erl_mk_atom("ok"));

BADARG:
    return erl_mk_atom("badarg");
}

/*
 * setns(2)
 *
 */
    static ETERM *
alcove_setns(ETERM *arg)
{
    ETERM *hd = NULL;
    char *path = NULL;
    int fd = -1;
    int rv = 0;

    /* path */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ALCOVE_IS_IOLIST(hd))
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        path = erl_iolist_to_string(hd);

    if (!path)
        goto BADARG;

    fd = open(path, O_RDONLY);
    if (fd < 0)
        return alcove_errno(errno);

    rv = setns(fd, 0);

    (void)close(fd);

    return ( (rv < 0) ? alcove_errno(errno) : erl_mk_atom("ok"));

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

    len = erl_length(arg);

    /* xargs --show-limits
     *
     * POSIX smallest allowable upper limit on argument length (all
     * systems): 4096
     */
    if (len < 0 || len >= 4096)
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

    static void
alcove_free_argv(char **argv)
{
    int i = 0;

    if (argv == NULL)
        return;

    for (i = 0; argv[i]; i++)
        free(argv[i]);

    free(argv);
}
