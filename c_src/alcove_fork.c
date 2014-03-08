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

static int alcove_stdio(alcove_fd_t *fd);
static int alcove_child_fun(void *arg);
static int alcove_parent_fd(alcove_state_t *ap, alcove_fd_t *fd, pid_t pid);

/*
 * fork(2)
 *
 */
    ETERM *
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
 * clone(2)
 *
 */
    ETERM *
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
 * setns(2)
 *
 */
    ETERM *
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
    ETERM *
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
 * clone flags
 *
 */
    ETERM *
alcove_clone_flags(alcove_state_t *ap, ETERM *arg)
{
    ETERM *hd = NULL;
    char *flag = NULL;

    /* flag */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_ATOM(hd))
        goto BADARG;

    flag = ERL_ATOM_PTR(hd);

    if      (!strncmp(flag, "newns", 5))    return erl_mk_int(CLONE_NEWNS);
    else if (!strncmp(flag, "newuts", 6))   return erl_mk_int(CLONE_NEWUTS);
    else if (!strncmp(flag, "newipc", 6))   return erl_mk_int(CLONE_NEWIPC);
    else if (!strncmp(flag, "newuser", 7))  return erl_mk_int(CLONE_NEWUSER);
    else if (!strncmp(flag, "newpid", 6))   return erl_mk_int(CLONE_NEWPID);
    else if (!strncmp(flag, "newnet", 6))   return erl_mk_int(CLONE_NEWNET);
    else return erl_mk_atom("false");

BADARG:
    return erl_mk_atom("badarg");
}

/*
 * Utility functions
 */
    int
alcove_stdio(alcove_fd_t *fd)
{
    if ( (pipe(fd->in) < 0)
            || (pipe(fd->out) < 0)
            || (pipe(fd->err) < 0))
        return -1;

    return 0;
}

    int
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

    int
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
