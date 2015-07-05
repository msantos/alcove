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
#include "alcove_fork.h"

#define PIPE_READ 0
#define PIPE_WRITE 1

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
        (void)alcove_close_pipe(fd->ctl);
        (void)alcove_close_pipe(fd->in);
        (void)alcove_close_pipe(fd->out);
        (void)alcove_close_pipe(fd->err);
        return -1;
    }

    return 0;
}

    int
alcove_set_cloexec(int fd)
{
    return alcove_setfd(fd, FD_CLOEXEC);
}

    int
alcove_close_pipe(int fd[2])
{
    if (alcove_close_fd(fd[0]) < 0)
        return -1;

    if (alcove_close_fd(fd[1]) < 0)
        return -1;

    return 0;
}

    int
alcove_close_fd(int fd)
{
    if (fd >= 0)
        return close(fd);

    return 0;
}

    int
alcove_child_fun(void *arg)
{
    alcove_arg_t *child_arg = arg;
    alcove_state_t *ap = child_arg->ap;
    alcove_stdio_t *fd = child_arg->fd;
    sigset_t *sigset = child_arg->sigset;
    int sigpipe[2] = {0};

    if (pipe(sigpipe) < 0)
        return -1;

    if ( (dup2(sigpipe[PIPE_READ], ALCOVE_SIGREAD_FILENO) < 0)
            || (dup2(sigpipe[PIPE_WRITE], ALCOVE_SIGWRITE_FILENO) < 0))
        return -1;

    if ( (alcove_setfd(ALCOVE_SIGREAD_FILENO, FD_CLOEXEC|O_NONBLOCK) < 0)
            || (alcove_setfd(ALCOVE_SIGWRITE_FILENO, FD_CLOEXEC|O_NONBLOCK) < 0))
        return -1;

    /* TODO ensure fd's do not overlap */
    if ( (dup2(fd->in[PIPE_READ], STDIN_FILENO) < 0)
            || (dup2(fd->out[PIPE_WRITE], STDOUT_FILENO) < 0)
            || (dup2(fd->err[PIPE_WRITE], STDERR_FILENO) < 0)
            || (dup2(fd->ctl[PIPE_READ], ALCOVE_FDCTL_FILENO) < 0))
        return -1;

    if ( (alcove_close_pipe(fd->in) < 0)
            || (alcove_close_pipe(fd->out) < 0)
            || (alcove_close_pipe(fd->err) < 0)
            || (alcove_close_pipe(fd->ctl) < 0)
            || (alcove_close_pipe(sigpipe) < 0))
        return -1;

    if (alcove_set_cloexec(ALCOVE_FDCTL_FILENO) < 0)
        return -1;

    if (pid_foreach(ap, 0, NULL, NULL, pid_not_equal, close_parent_fd) < 0)
        return -1;

    ap->depth++;

    if (sigprocmask(SIG_SETMASK, sigset, NULL) < 0)
        return -1;

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
        err(errno, "close");

    if ( (alcove_set_cloexec(fd->ctl[PIPE_WRITE]) < 0)
            || (alcove_set_cloexec(fd->in[PIPE_WRITE]) < 0)
            || (alcove_set_cloexec(fd->out[PIPE_READ]) < 0)
            || (alcove_set_cloexec(fd->err[PIPE_READ]) < 0))
        err(errno, "alcove_set_cloexec");

    return pid_foreach(ap, 0, fd, &pid, pid_equal, stdio_pid);
}

    int
avail_pid(alcove_state_t *ap, alcove_child_t *c, void *arg1, void *arg2)
{
    /* slot found */
    if (c->pid == 0)
        return 0;

    return 1;
}

    int
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

    int
close_parent_fd(alcove_state_t *ap, alcove_child_t *c, void *arg1, void *arg2)
{
    (void)alcove_close_fd(c->fdctl);
    (void)alcove_close_fd(c->fdin);
    (void)alcove_close_fd(c->fdout);
    (void)alcove_close_fd(c->fderr);

    return 1;
}
