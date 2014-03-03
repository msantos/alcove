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
#include "err.h"

#include <signal.h>

#define ALCOVE_MSG_CALL  0
#define ALCOVE_MSG_CAST (htons(1))
#define ALCOVE_MSG_CHILDIN (htons(2))
#define ALCOVE_MSG_CHILDOUT (htons(3))
#define ALCOVE_MSG_CHILDERR (htons(4))

#define PIPE_READ 0
#define PIPE_WRITE 1

#ifdef CLONE_NEWNS
#pragma message "Support for namespaces using clone(2) enabled"
#else
#pragma message "Support for namespaces using clone(2) disabled"
#endif

typedef struct {
    int ctl[2];
    int in[2];
    int out[2];
    int err[2];
} alcove_fd_t;

static int alcove_fork(alcove_state_t *);

static void alcove_ctl(int fd);
static void alcove_select(alcove_state_t *);
static alcove_msg_t *alcove_msg(int);
static void usage(alcove_state_t *);

static int alcove_fork_child(void *arg);
static int alcove_fork_parent(alcove_state_t *ap, alcove_fd_t *fd);

static ssize_t alcove_child_proxy(int, int, u_int16_t);
static int alcove_write(int fd, u_int16_t, ETERM *);
static ssize_t alcove_read(int, void *, ssize_t);

static void alcove_stats(alcove_state_t *ap);

extern char *__progname;

static int child_exited = 0;

    static void
gotsig(int sig)
{
    switch (sig) {
        case SIGCHLD:
            child_exited = 1;
            break;
        default:
            break;
    }
}

    int
main(int argc, char *argv[])
{
    alcove_state_t *ap = NULL;
    int ch = 0;

    erl_init(NULL, 0);

    ap = calloc(1, sizeof(alcove_state_t));
    if (!ap)
        erl_err_sys("calloc");

    ap->pid = -1;
    ap->ctl = -1;
    ap->fdin = -1;
    ap->fdout = -1;
    ap->fderr = -1;

    signal(SIGCHLD, gotsig);

    while ( (ch = getopt(argc, argv, "n:hv")) != -1) {
        switch (ch) {
            case 'n':
#ifdef CLONE_NEWNS
                if (!strncmp("ipc", optarg, 3))         ap->ns |= CLONE_NEWIPC;
                else if (!strncmp("net", optarg, 3))    ap->ns |= CLONE_NEWNET;
                else if (!strncmp("ns", optarg, 2))     ap->ns |= CLONE_NEWNS;
                else if (!strncmp("pid", optarg, 3))    ap->ns |= CLONE_NEWPID;
                else if (!strncmp("uts", optarg, 3))    ap->ns |= CLONE_NEWUTS;
                else if (!strncmp("user", optarg, 4))    ap->ns |= CLONE_NEWUSER;
                else usage(ap);
#else
                usage(ap);
#endif
                break;
            case 'v':
                ap->verbose++;
                break;
            case 'h':
            default:
                usage(ap);
        }
    }

    if (alcove_fork(ap) < 0)
        exit (EXIT_FAILURE);

    alcove_select(ap);
    exit(0);
}

    static int
alcove_fork(alcove_state_t *ap)
{
    alcove_fd_t fd = {{0}};

#ifdef CLONE_NEWNS
    const size_t stack_size = 65536;
    char *child_stack = NULL;

    child_stack = calloc(stack_size, 1);
    if (!child_stack)
        erl_err_sys("calloc");
#endif

    if (socketpair(AF_UNIX, SOCK_STREAM, 0, fd.ctl) < 0)
        erl_err_sys("socketpair");

    if ( (pipe(fd.in) < 0)
            || (pipe(fd.out) < 0)
            || (pipe(fd.err) < 0))
        erl_err_sys("pipe");

#ifdef CLONE_NEWNS
    ap->pid = clone(alcove_fork_child, child_stack + stack_size, ap->ns | SIGCHLD, &fd);
    if (ap->pid < 0)
        erl_err_sys("clone");

    (void)alcove_fork_parent(ap, &fd);
#else
    ap->pid = fork();

    switch (ap->pid) {
        case -1:
            erl_err_sys("fork");
        case 0:
            (void)alcove_fork_child(ap);
            break;
        default:
            (void)alcove_fork_parent(ap, &fd);
            break;
    }
#endif

    return 0;
}

    static int
alcove_fork_child(void *arg)
{
    alcove_fd_t *fd = arg;

    if ( (close(fd->ctl[PIPE_WRITE]) < 0)
            || (close(fd->in[PIPE_WRITE]) < 0)
            || (close(fd->out[PIPE_READ]) < 0)
            || (close(fd->err[PIPE_READ]) < 0))
        erl_err_sys("close");

    if ( (dup2(fd->in[PIPE_READ], STDIN_FILENO) < 0)
            || (dup2(fd->out[PIPE_WRITE], STDOUT_FILENO) < 0)
            || (dup2(fd->err[PIPE_WRITE], STDERR_FILENO) < 0))
        erl_err_sys("dup2");

    alcove_ctl(fd->ctl[PIPE_READ]);

    return 0;
}

    static int
alcove_fork_parent(alcove_state_t *ap, alcove_fd_t *fd)
{
    if ( (close(fd->ctl[PIPE_READ]) < 0)
            || (close(fd->in[PIPE_READ]) < 0)
            || (close(fd->out[PIPE_WRITE]) < 0)
            || (close(fd->err[PIPE_WRITE]) < 0))
        erl_err_sys("close");

    ap->ctl = fd->ctl[PIPE_WRITE];
    ap->fdin = fd->in[PIPE_WRITE];
    ap->fdout = fd->out[PIPE_READ];
    ap->fderr = fd->err[PIPE_READ];

    return 0;
}


    static void
alcove_ctl(int fd)
{
    alcove_msg_t *msg = NULL;
    ETERM *arg = NULL;
    ETERM *reply = NULL;

    for ( ; ; ) {
        msg = alcove_msg(fd);
        if (!msg)
            break;

        arg = erl_decode(msg->arg);
        if (!arg)
            erl_err_quit("invalid message");

        reply = alcove_cmd(msg->cmd, arg);
        if (!reply)
            erl_err_quit("unrecoverable error");

        free(msg->arg);
        free(msg);
        erl_free_compound(arg);

        if (alcove_write(fd, ALCOVE_MSG_CALL, reply) < 0)
            erl_err_sys("alcove_write");

        erl_free_compound(reply);
    }
}

    static void
alcove_select(alcove_state_t *ap)
{
    fd_set rfds;
    int fdmax = 0;
    int rv = 0;

    for ( ; ; ) {
        if (child_exited)
            return;

        FD_ZERO(&rfds);

        FD_SET(STDIN_FILENO, &rfds);

        fdmax = STDIN_FILENO;

        if (ap->fdout > -1) {
            FD_SET(ap->fdout, &rfds);
            fdmax = MAX(fdmax, ap->fdout);
        }

        if (ap->fderr > -1) {
            FD_SET(ap->fderr, &rfds);
            fdmax = MAX(fdmax, ap->fderr);
        }

        if (ap->ctl > -1) {
            FD_SET(ap->ctl, &rfds);
            fdmax = MAX(fdmax, ap->ctl);
        }

        rv = select(fdmax+1, &rfds, NULL, NULL, NULL);

        if (rv < 0) {
            switch (errno) {
                case EAGAIN:
                case EINTR:
                    continue;
            default:
                break;
            }
        }

        if (FD_ISSET(ap->ctl, &rfds)) {
            ssize_t n = 0;
            char buf[65535] = {0};

            n = read(ap->ctl, buf, sizeof(buf));

            if (n == 0) {
                ap->ctl = -1;
            }
            else if (n < 0) {
                erl_err_quit("eof");
            }

            /* XXX lock stdout */
            if (write(STDOUT_FILENO, buf, n) != n)
                erl_err_sys("write:ctl->stdout");
        }

        if (FD_ISSET(STDIN_FILENO, &rfds)) {
            u_int16_t bufsz = 0;
            u_int16_t type = 0;
            char buf[65535] = {0};

            errno = 0;

            /* total length, not including length header */
            if (alcove_read(STDIN_FILENO, &bufsz, sizeof(bufsz)) != sizeof(bufsz)) {
                if (errno == 0)
                    return;

                erl_err_sys("read:stdin");
            }

            /* message type */
            if (alcove_read(STDIN_FILENO, &type, sizeof(type)) != sizeof(type))
                erl_err_sys("read:stdin");

            bufsz = ntohs(bufsz);
            bufsz -= sizeof(type);

            if (alcove_read(STDIN_FILENO, buf, bufsz) != bufsz)
                erl_err_sys("read:stdin");

            if (type == ALCOVE_MSG_CALL) {
                u_int16_t size = htons(bufsz);
                if ( (write(ap->ctl, &size, 2) != 2)
                        || (write(ap->ctl, buf, bufsz) != bufsz))
                    erl_err_sys("write:stdin->ctl");
            }
            else if (type == ALCOVE_MSG_CHILDIN) {
                if (write(ap->fdin, buf, bufsz) != bufsz)
                    erl_err_sys("write:stdin->childin");
            }
            else {
                /* XXX return badarg to caller */
                erl_err_quit("bad message type:%u", type);
            }
        }

        if (FD_ISSET(ap->fdout, &rfds)) {
            switch (alcove_child_proxy(ap->fdout, STDOUT_FILENO, ALCOVE_MSG_CHILDOUT)) {
                case -1:
                    erl_err_sys("alcove_child_proxy");
                case 0:
                    return;
                default:
                    break;
            }
        }

        if (FD_ISSET(ap->fderr, &rfds)) {
            switch (alcove_child_proxy(ap->fderr, STDOUT_FILENO, ALCOVE_MSG_CHILDERR)) {
                case -1:
                    erl_err_sys("alcove_child_proxy");
                case 0:
                    return;
                default:
                    break;
            }
        }
    }
}

    static alcove_msg_t *
alcove_msg(int fd)
{
    ssize_t n = 0;
    u_int16_t buf = 0;
    u_int16_t len = 0;
    alcove_msg_t *msg = NULL;

    errno = 0;
    n = alcove_read(fd, &buf, sizeof(buf));

    if (n != sizeof(buf)) {
        if (errno == 0)
            return NULL;

        erl_err_sys("alcove_msg: expected=%lu, got=%lu",
                (unsigned long)sizeof(buf),
                (unsigned long)n);
    }

    len = ntohs(buf);

    if (len >= UINT16_MAX || len < sizeof(buf))
        erl_err_quit("alcove_msg: invalid len=%d (max=%d)", len, UINT16_MAX);

    len -= sizeof(buf);

    msg = alcove_malloc(sizeof(alcove_msg_t));
    msg->arg = alcove_malloc(len);

    /* cmd */
    n = alcove_read(fd, &buf, sizeof(buf));
    if (n != sizeof(buf))
        erl_err_sys("alcove_msg: expected=%lu, got=%lu",
                (unsigned long)sizeof(buf),
                (unsigned long)n);

    msg->cmd = ntohs(buf);

    /* arg */
    n = alcove_read(fd, msg->arg, len);
    if (n != len)
        erl_err_sys("alcove_msg: expected=%u, got=%lu",
                len, (unsigned long)n);

    return msg;
}

    static ssize_t
alcove_child_proxy(int fdin, int fdout, u_int16_t type)
{
    ssize_t n = 0;
    char buf[65535] = {0};

    errno = 0;
    n = read(fdin, buf, sizeof(buf));

    if (n <= 0) {
        if (errno == 0)
            return 0;

        return -1;
    }

    if (alcove_write(fdout, type, erl_mk_binary(buf,n)) < 0)
        erl_err_sys("alcove_write");

    return n;
}

    int
alcove_send(ETERM *t)
{
    return alcove_write(STDOUT_FILENO, ALCOVE_MSG_CAST, t);
}

    static int
alcove_write(int fd, u_int16_t type, ETERM *t)
{
    int tlen = 0;
    u_int16_t hlen = 0;
    unsigned char *buf = NULL;

    tlen = erl_term_len(t);
    if (tlen < 0 || tlen+sizeof(type) >= UINT16_MAX)
        goto ERR;

    hlen = ntohs(tlen+sizeof(type));

    buf = alcove_malloc(tlen);

    if (erl_encode(t, buf) < 1)
        goto ERR;

//    flockfile(stdout);

    if ( (write(fd, &hlen, 2) != 2) ||
         (write(fd, &type, 2) != 2) ||
         (write(fd, buf, tlen) != tlen))
        goto ERR;

//    funlockfile(stdout);

    erl_free(buf);
    return 0;

ERR:
    erl_free(buf);
    return -1;
}

    static ssize_t
alcove_read(int fd, void *buf, ssize_t len)
{
    ssize_t i = 0;
    ssize_t got = 0;

    do {
        if ((i = read(fd, buf + got, len - got)) <= 0)
            return(i);
        got += i;
    } while (got < len);

    return len;
}

    static void
alcove_stats(alcove_state_t *ap)
{
    unsigned long allocated = 0;
    unsigned long freed = 0;

    erl_eterm_statistics(&allocated, &freed);
    VERBOSE(0, "allocated=%ld, freed=%ld", allocated, freed);
    erl_eterm_release();
}

    static void
usage(alcove_state_t *ap)
{
    (void)fprintf(stderr, "%s %s\n",
            __progname, ALCOVE_VERSION);
    (void)fprintf(stderr,
            "usage: %s <options>\n"
#ifdef CLONE_NEWNS
            "   -n <namespace>  new namespace: ipc, net, ns, pid, user, uts\n"
#endif
            "   -v              verbose mode\n",
            __progname
            );

    exit (EXIT_FAILURE);
}
