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
#include "alcove_cmd.h"

#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <err.h>
#include <errno.h>

#include <sys/types.h>

#define PIPE_READ 0
#define PIPE_WRITE 1

typedef struct {
    int ctl[2];
    int in[2];
    int out[2];
    int err[2];
} alcove_fd_t;

typedef struct {
    alcove_state_t *ap;
    alcove_fd_t *fd;
} alcove_arg_t;

static char **alcove_list_to_argv(ETERM *);
static void alcove_free_argv(char **);

static int alcove_stdio(alcove_fd_t *fd);
static int alcove_child_fun(void *arg);
static int alcove_parent_fd(alcove_state_t *ap, alcove_fd_t *fd, pid_t pid);

#define ALCOVE_IS_IOLIST(_t)  (ERL_IS_BINARY(_t) || ERL_IS_LIST(_t))

    ETERM *
alcove_cmd(alcove_state_t *ap, u_int32_t cmd, ETERM *arg)
{
    alcove_cmd_t *fun = NULL;

    if (cmd >= sizeof(cmds)/sizeof(cmds[0]))
        return erl_mk_atom("badarg");

    fun = &cmds[cmd];

    if (!ERL_IS_LIST(arg) || erl_length(arg) != fun->narg)
        return erl_mk_atom("badarg");

    return (*fun->fp)(ap, arg);
}

    static ETERM *
alcove_version(alcove_state_t *ap, ETERM *arg)
{
    return alcove_bin(ALCOVE_VERSION);
}

    static ETERM *
alcove_pid(alcove_state_t *ap, ETERM *arg)
{
    ETERM *t = erl_mk_empty_list();
    int i = 0;

    for (i = 0; i < ALCOVE_MAX_CHILD; i++) {
        if (ap->child[i].pid <= 0)
            continue;

        t = erl_cons(erl_mk_int(ap->child[i].pid), t);
    }

    return t;
}

/*
 * chdir(2)
 *
 */
    static ETERM *
alcove_chdir(alcove_state_t *ap, ETERM *arg)
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
alcove_chroot(alcove_state_t *ap, ETERM *arg)
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
 * clone(2)
 *
 */
    static ETERM *
alcove_clone(alcove_state_t *ap, ETERM *arg)
{
    ETERM *hd = NULL;
    alcove_arg_t child_arg = {0};
    alcove_fd_t fd = {{0}};
    const size_t stack_size = 1024 * 1024;
    char *child_stack = NULL;
    int flags = 0;
    pid_t pid = 0;

    if (ap->nchild >= ALCOVE_MAX_CHILD - 1)
        return alcove_errno(EAGAIN);

    /* flags */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_INTEGER(hd))
        goto BADARG;

    flags = ERL_INT_UVALUE(hd);

    child_stack = calloc(stack_size, 1);
    if (!child_stack)
        return alcove_errno(errno);

    if (alcove_stdio(&fd) < 0)
        return alcove_errno(errno);

    child_arg.ap = ap;
    child_arg.fd = &fd;

    pid = clone(alcove_child_fun, child_stack + stack_size, flags | SIGCHLD, &child_arg);

    if (pid < 0)
        return alcove_errno(errno);

    free(child_stack);

    if (alcove_parent_fd(ap, &fd, pid) < 0)
        return alcove_errno(errno);

    return alcove_ok(erl_mk_int(pid));

BADARG:
    return erl_mk_atom("badarg");
}

/*
 * execvp(3)
 *
 */
    static ETERM *
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
 * fork(2)
 *
 */
    static ETERM *
alcove_fork(alcove_state_t *ap, ETERM *arg)
{
    alcove_arg_t child_arg = {0};
    alcove_fd_t fd = {{0}};
    pid_t pid = 0;

    if (ap->nchild >= ALCOVE_MAX_CHILD - 1)
        return alcove_errno(EAGAIN);

    if (alcove_stdio(&fd) < 0)
        return alcove_errno(errno);

    child_arg.ap = ap;
    child_arg.fd = &fd;

    pid = fork();

    switch (pid) {
        case -1:
            return alcove_errno(errno);
        case 0:
            (void)alcove_child_fun(&child_arg);
            erl_err_sys("fork");
        default:
            if (alcove_parent_fd(ap, &fd, pid) < 0)
                return alcove_errno(errno);

            return alcove_ok(erl_mk_int(pid));
    }
}


/*
 * getcwd(3)
 *
 */
    static ETERM *
alcove_getcwd(alcove_state_t *ap, ETERM *arg)
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
alcove_getgid(alcove_state_t *ap, ETERM *arg)
{
    return erl_mk_uint(getgid());
}

/*
 * getpid(2)
 *
 */
    static ETERM *
alcove_getpid(alcove_state_t *ap, ETERM *arg)
{
    return erl_mk_int(getpid());
}

/*
 * getuid(2)
 *
 */
    static ETERM *
alcove_getuid(alcove_state_t *ap, ETERM *arg)
{
    return erl_mk_uint(getuid());
}

/*
 * setgid(2)
 *
 */
    static ETERM *
alcove_setgid(alcove_state_t *ap, ETERM *arg)
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
alcove_setuid(alcove_state_t *ap, ETERM *arg)
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
 * gethostname(2)
 *
 */
    static ETERM *
alcove_gethostname(alcove_state_t *ap, ETERM *arg)
{
    char name[HOST_NAME_MAX] = {0};
    int rv = 0;

    rv = gethostname(name, HOST_NAME_MAX-1);

    return (rv < 0)
        ? alcove_errno(errno)
        : alcove_ok(erl_mk_binary(name, strlen(name)));
}

/*
 * sethostname(2)
 *
 */
    static ETERM *
alcove_sethostname(alcove_state_t *ap, ETERM *arg)
{
    ETERM *hd = NULL;
    char *name = NULL;
    int rv = 0;

    /* hostname */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ALCOVE_IS_IOLIST(hd))
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        name = erl_iolist_to_string(hd);

    if (!name)
        goto BADARG;

    rv = sethostname(name, strlen(name)+1);

    erl_free(name);

    return ( (rv < 0) ? alcove_errno(errno) : erl_mk_atom("ok"));

BADARG:
    erl_free(name);
    return erl_mk_atom("badarg");
}

/*
 * getrlimit(2)
 *
 */
    static ETERM *
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
    static ETERM *
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
 * setns(2)
 *
 */
    static ETERM *
alcove_setns(alcove_state_t *ap, ETERM *arg)
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
 * unshare(2)
 *
 */
    static ETERM *
alcove_unshare(alcove_state_t *ap, ETERM *arg)
{
    ETERM *hd = NULL;
    int flags = 0;
    int rv = 0;

    /* flags */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_INTEGER(hd))
        goto BADARG;

    flags = ERL_INT_VALUE(hd);

    rv = unshare(flags);

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

    static int
alcove_stdio(alcove_fd_t *fd)
{
    if ( (pipe(fd->in) < 0)
            || (pipe(fd->out) < 0)
            || (pipe(fd->err) < 0))
        return -1;

    return 0;
}

    static int
alcove_child_fun(void *arg)
{
    alcove_arg_t *child_arg = arg;
    alcove_state_t *ap = child_arg->ap;
    alcove_fd_t *fd = child_arg->fd;

    if ( (close(fd->in[PIPE_WRITE]) < 0)
            || (close(fd->out[PIPE_READ]) < 0)
            || (close(fd->err[PIPE_READ]) < 0))
        return -1;

    if ( (dup2(fd->in[PIPE_READ], STDIN_FILENO) < 0)
            || (dup2(fd->out[PIPE_WRITE], STDOUT_FILENO) < 0)
            || (dup2(fd->err[PIPE_WRITE], STDERR_FILENO) < 0))
        return -1;

    alcove_ctl(ap);

    return 0;
}

    static int
alcove_parent_fd(alcove_state_t *ap, alcove_fd_t *fd, pid_t pid)
{
    int i = 0;

    if ( (close(fd->in[PIPE_READ]) < 0)
            || (close(fd->out[PIPE_WRITE]) < 0)
            || (close(fd->err[PIPE_WRITE]) < 0))
        return -1;

    ap->nchild++;

    for (i = 0; i < ALCOVE_MAX_CHILD; i++) {
        if (ap->child[i].pid)
            continue;

        ap->child[i].pid = pid;
        ap->child[i].fdin = fd->in[PIPE_WRITE];
        ap->child[i].fdout = fd->out[PIPE_READ];
        ap->child[i].fderr = fd->err[PIPE_READ];

        break;
    }

    return 0;
}
