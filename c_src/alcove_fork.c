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
#ifdef __linux__
#define _GNU_SOURCE
#include <sched.h>
#ifndef HAVE_SETNS
#include <syscall.h>
#endif
#endif

#include "alcove.h"
#include "alcove_call.h"
#include "alcove_fork.h"

#define PIPE_READ 0
#define PIPE_WRITE 1

typedef struct {
    int ctl[2];
    int in[2];
    int out[2];
    int err[2];
} alcove_stdio_t;

typedef struct {
    alcove_state_t *ap;
    alcove_stdio_t *fd;
} alcove_arg_t;

static int alcove_stdio(alcove_stdio_t *fd);
static int alcove_set_cloexec(int fd);
static void alcove_close_pipe(int fd[2]);
static int alcove_child_fun(void *arg);
static int alcove_parent_fd(alcove_state_t *ap, alcove_stdio_t *fd, pid_t pid);
static int avail_pid(alcove_state_t *ap, alcove_child_t *c,
        void *arg1, void *arg2);
static int stdio_pid(alcove_state_t *ap, alcove_child_t *c,
        void *arg1, void *arg2);

#ifdef __linux__
#ifndef HAVE_SETNS
static int setns(int fd, int nstype);
#endif
#endif

/*
 * fork(2)
 *
 */
    ssize_t
alcove_fork(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int rindex = 0;

    alcove_arg_t child_arg = {0};
    alcove_stdio_t fd = {{0}};
    pid_t pid = 0;

    if (ap->depth >= ap->maxforkdepth)
        return alcove_mk_errno(reply, rlen, EAGAIN);

    if (pid_foreach(ap, 0, NULL, NULL, pid_equal, avail_pid) != 0)
        return alcove_mk_errno(reply, rlen, EAGAIN);

    if (alcove_stdio(&fd) < 0)
        return alcove_mk_errno(reply, rlen, errno);

    child_arg.ap = ap;
    child_arg.fd = &fd;

    pid = fork();

    switch (pid) {
        case -1:
            return alcove_mk_errno(reply, rlen, errno);
        case 0:
            (void)alcove_child_fun(&child_arg);
            err(EXIT_FAILURE, "fork");
        default:
            if (alcove_parent_fd(ap, &fd, pid) < 0)
                return alcove_mk_errno(reply, rlen, errno);

            ALCOVE_OK(reply, &rindex,
                alcove_encode_long(reply, rlen, &rindex, pid));

            return rindex;
    }
}

/*
 * clone(2)
 *
 */
    ssize_t
alcove_clone(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
#ifdef __linux__
    int index = 0;
    int rindex = 0;

    alcove_arg_t child_arg = {0};
    alcove_stdio_t fd = {{0}};
    struct rlimit stack_size = {0};
    char *child_stack = NULL;
    int flags = 0;
    pid_t pid = 0;
    int errnum = 0;

    if (ap->depth >= ap->maxforkdepth)
        return alcove_mk_errno(reply, rlen, EAGAIN);

    if (pid_foreach(ap, 0, NULL, NULL, pid_equal, avail_pid) != 0)
        return alcove_mk_errno(reply, rlen, EAGAIN);

    /* flags */
    if (alcove_decode_int(arg, len, &index, &flags) < 0)
        return -1;

    if (getrlimit(RLIMIT_STACK, &stack_size) < 0)
        return alcove_mk_errno(reply, rlen, errno);

    child_stack = calloc(stack_size.rlim_cur, 1);
    if (!child_stack)
        return alcove_mk_errno(reply, rlen, errno);

    if (alcove_stdio(&fd) < 0)
        goto ERR;

    child_arg.ap = ap;
    child_arg.fd = &fd;

    pid = clone(alcove_child_fun, child_stack + stack_size.rlim_cur,
            flags | SIGCHLD, &child_arg);

    if (pid < 0)
        goto ERR;

    free(child_stack);

    if (alcove_parent_fd(ap, &fd, pid) < 0)
        return alcove_mk_errno(reply, rlen, errno);

    ALCOVE_OK(reply, &rindex,
        alcove_encode_long(reply, rlen, &rindex, pid)
    );

    return rindex;

ERR:
    errnum = errno;
    free(child_stack);
    return alcove_mk_errno(reply, rlen, errnum);
#else
    return alcove_mk_error(reply, rlen, "unsupported");
#endif
}

/*
 * setns(2)
 *
 */
#ifdef __linux__
#ifndef HAVE_SETNS
    static int
setns(int fd, int nstype)
{
#ifdef __NR_setns
    return syscall(__NR_setns, fd, nstype);
#else
    errno = ENOSYS;
    return -1;
#endif
}
#endif
#endif

    ssize_t
alcove_setns(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
#ifdef __linux__
    int index = 0;

    char path[PATH_MAX] = {0};
    size_t plen = sizeof(path)-1;
    int fd = -1;
    int rv = 0;
    int errnum = 0;

    /* path */
    if (alcove_decode_iolist(arg, len, &index, path, &plen) < 0 ||
            plen == 0)
        return -1;

    fd = open(path, O_RDONLY);
    if (fd < 0)
        return alcove_mk_errno(reply, rlen, errno);

    rv = setns(fd, 0);

    errnum = errno;

    (void)close(fd);

    if (rv < 0)
        return alcove_mk_errno(reply, rlen, errnum);

    return alcove_mk_atom(reply, rlen, "ok");
#else
    return alcove_mk_error(reply, rlen, "unsupported");
#endif
}

/*
 * unshare(2)
 *
 */
    ssize_t
alcove_unshare(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
#ifdef __linux__
    int index = 0;

    int flags = 0;

    /* flags */
    if (alcove_decode_int(arg, len, &index, &flags) < 0)
        return -1;

    return (unshare(flags) < 0)
        ? alcove_mk_errno(reply, rlen, errno)
        : alcove_mk_atom(reply, rlen, "ok");
#else
    return alcove_mk_error(reply, rlen, "unsupported");
#endif
}

/*
 * clone flags
 *
 */
    ssize_t
alcove_clone_define(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
#ifdef __linux__
    int index = 0;
    int rindex = 0;

    char flag[MAXATOMLEN] = {0};

    /* flag */
    if (alcove_decode_atom(arg, len, &index, flag) < 0)
        return -1;

    ALCOVE_ERR(alcove_encode_version(reply, rlen, &rindex));
    ALCOVE_ERR(alcove_define(reply, rlen, &rindex,
                flag, alcove_clone_constants));
    return rindex;
#else
    return alcove_mk_atom(reply, rlen, "false");
#endif
}

/*
 * Utility functions
 */
    int
alcove_stdio(alcove_stdio_t *fd)
{
    if (socketpair(AF_UNIX, SOCK_STREAM, 0, fd->ctl) < 0)
        return -1;

    if ( (pipe(fd->in) < 0)
            || (pipe(fd->out) < 0)
            || (pipe(fd->err) < 0)) {
        alcove_close_pipe(fd->ctl);
        alcove_close_pipe(fd->in);
        alcove_close_pipe(fd->out);
        alcove_close_pipe(fd->err);
        return -1;
    }

    return 0;
}

    static int
alcove_set_cloexec(int fd)
{
    int flags = 0;

    flags = fcntl(fd, F_GETFD, 0);
    if (flags < 0)
        return -1;

    return fcntl(fd, F_SETFD, flags | FD_CLOEXEC);
}

    static void
alcove_close_pipe(int fd[2])
{
    /* fd should be above 0 since STDIN_FILENO is still open */
    if (fd[0] > 0)
        (void)close(fd[0]);

    if (fd[1] > 0)
        (void)close(fd[1]);
}

    int
alcove_child_fun(void *arg)
{
    alcove_arg_t *child_arg = arg;
    alcove_state_t *ap = child_arg->ap;
    alcove_stdio_t *fd = child_arg->fd;
    long maxfd = sysconf(_SC_OPEN_MAX);
    int n = 0;

    if ( (close(fd->ctl[PIPE_WRITE]) < 0)
            || (close(fd->in[PIPE_WRITE]) < 0)
            || (close(fd->out[PIPE_READ]) < 0)
            || (close(fd->err[PIPE_READ]) < 0))
        return -1;

    if ( (dup2(fd->in[PIPE_READ], STDIN_FILENO) < 0)
            || (dup2(fd->out[PIPE_WRITE], STDOUT_FILENO) < 0)
            || (dup2(fd->err[PIPE_WRITE], STDERR_FILENO) < 0)
            || (dup2(fd->ctl[PIPE_READ], ALCOVE_FDCTL) < 0))
        return -1;

    /* Close all other fd's inherited from the parent. */
    for (n = 0; n < maxfd; n++) {
        if ( (n != ALCOVE_FDCTL)
                && (n != STDIN_FILENO)
                && (n != STDOUT_FILENO)
                && (n != STDERR_FILENO))
            (void)close(n);
    }

    if (alcove_set_cloexec(ALCOVE_FDCTL) < 0)
        return -1;

    ap->depth++;
    alcove_event_loop(ap);

    return 0;
}

    int
alcove_parent_fd(alcove_state_t *ap, alcove_stdio_t *fd, pid_t pid)
{
    /* What to do if close(2) fails here?
     *
     * The options are ignore the failure, kill the child process and
     * return errno or exit (the child will be forced to exit as well
     * when stdin is closed).
     */
    if ( (close(fd->ctl[PIPE_READ]) < 0)
            || (close(fd->in[PIPE_READ]) < 0)
            || (close(fd->out[PIPE_WRITE]) < 0)
            || (close(fd->err[PIPE_WRITE]) < 0))
        err(EXIT_FAILURE, "alcove_parent_fd:close");

    return pid_foreach(ap, 0, fd, &pid, pid_equal, stdio_pid);
}

    static int
avail_pid(alcove_state_t *ap, alcove_child_t *c, void *arg1, void *arg2)
{
    /* slot found */
    if (c->pid == 0)
        return 0;

    return 1;
}

    static int
stdio_pid(alcove_state_t *ap, alcove_child_t *c, void *arg1, void *arg2)
{
    alcove_stdio_t *fd = arg1;
    pid_t *pid = arg2;

    c->pid = *pid;
    c->fdctl = fd->ctl[PIPE_WRITE];
    c->fdin = fd->in[PIPE_WRITE];
    c->fdout = fd->out[PIPE_READ];
    c->fderr = fd->err[PIPE_READ];

    return 0;
}
