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

#include <poll.h>
#include <sys/wait.h>

enum {
    ALCOVE_MSG_STDIN = 0,
    ALCOVE_MSG_STDOUT,
    ALCOVE_MSG_STDERR,
    ALCOVE_MSG_PROXY,
    ALCOVE_MSG_CALL,
    ALCOVE_MSG_EVENT,
};

#define ALCOVE_CHILD_EXEC -2

#define ALCOVE_MSG_TYPE(s) \
    ((s->fdctl == ALCOVE_CHILD_EXEC) ? ALCOVE_MSG_STDOUT : ALCOVE_MSG_PROXY)

#define get_int32(s) ((((unsigned char*) (s))[0] << 24) | \
                      (((unsigned char*) (s))[1] << 16) | \
                      (((unsigned char*) (s))[2] << 8)  | \
                      (((unsigned char*) (s))[3]))

#define get_int16(s) ((((unsigned char*)  (s))[0] << 8) | \
                      (((unsigned char*)  (s))[1]))

#ifdef __linux__
#pragma message "Support for namespaces using clone(2) enabled"
#else
#pragma message "Support for namespaces using clone(2) disabled"
#endif

static int alcove_stdin(alcove_state_t *ap);
static ssize_t alcove_msg_call(alcove_state_t *ap, unsigned char *buf,
        u_int16_t buflen);

static ssize_t alcove_child_stdio(int fdin, u_int16_t depth,
        alcove_child_t *c, u_int16_t type);
static ssize_t alcove_call_reply(u_int16_t, ETERM *);
static ssize_t alcove_call_fake_reply(pid_t pid, u_int16_t type, ETERM *t);

static int alcove_get_uint16(int fd, u_int16_t *val);
static ssize_t alcove_read(int, void *, ssize_t);

static int exited_pid(alcove_state_t *ap, alcove_child_t *c,
        void *arg1, void *arg2);
static int set_pid(alcove_state_t *ap, alcove_child_t *c,
        void *arg1, void *arg2);
static int write_to_pid(alcove_state_t *ap, alcove_child_t *c,
        void *arg1, void *arg2);
static int read_from_pid(alcove_state_t *ap, alcove_child_t *c,
        void *arg1, void *arg2);

static int alcove_handle_signal(alcove_state_t *ap);

static void alcove_stats(alcove_state_t *ap);
static void usage(alcove_state_t *);

extern char *__progname;

u_int64_t sigcaught = 0;

    void
sighandler(int sig)
{
    if (sig < sizeof(sigcaught) * 8) {
        sigcaught ^= (1 << sig);
    }
}

    int
main(int argc, char *argv[])
{
    alcove_state_t *ap = NULL;
    int ch = 0;
    struct sigaction act = {{0}};

    erl_init(NULL, 0);

    ap = calloc(1, sizeof(alcove_state_t));
    if (!ap)
        erl_err_sys("calloc");

    act.sa_handler = sighandler;
    if (sigaction(SIGCHLD, &act, NULL) < 0)
        erl_err_sys("sigaction");

    ap->maxfd = sysconf(_SC_OPEN_MAX);
    ap->maxchild = ap->maxfd / 4 - 4;
    ap->maxforkdepth = MAXFORKDEPTH;

    while ( (ch = getopt(argc, argv, "ae:m:M:hs:S:v")) != -1) {
        switch (ch) {
            case 'e':
                ALCOVE_SETOPT(ap, alcove_opt_exit_status, atoi(optarg));
                break;
            case 'm':
                ap->maxchild = (u_int16_t)atoi(optarg);
                break;
            case 'M':
                ap->maxforkdepth = (u_int16_t)atoi(optarg);
                break;
            case 's':
                ALCOVE_SETOPT(ap, alcove_opt_sigchld, atoi(optarg));
                break;
            case 'S':
                ALCOVE_SETOPT(ap, alcove_opt_termsig, atoi(optarg));
                break;
            case 'v':
                ap->verbose++;
                break;
            case 'h':
            default:
                usage(ap);
        }
    }

    ap->fdsetsize = ap->maxchild;

    ap->child = calloc(ap->fdsetsize, sizeof(alcove_child_t));
    if (!ap->child)
        erl_err_sys("calloc");

    /* Unlike the child processes, the port does not use a control fd.
     * An fd is acquired and leaked here to prevent calls to open()
     * at the port level from returning an uncloseable fd.
     *
     * The fd may have been opened by another program. For example,
     * valgrind will use the first available fd for the log file.
     */
    if ( (fcntl(ALCOVE_FDCTL, F_GETFD) < 0)
            && (open("/dev/null", O_RDWR) != ALCOVE_FDCTL))
            erl_err_quit("could not acquire ctl fd");

    alcove_event_loop(ap);
    exit(0);
}

    void
alcove_event_loop(alcove_state_t *ap)
{
    struct pollfd *fds = NULL;

    sigcaught = 0;

    if (ap->fdsetsize != ap->maxchild) {
        /* the array may be shrinking */
        (void)memset(ap->child, 0, sizeof(alcove_child_t) * ap->fdsetsize);

        ap->fdsetsize = ap->maxchild;
        ap->child = realloc(ap->child, sizeof(alcove_child_t) * ap->fdsetsize);

        if (!ap->child)
            erl_err_sys("realloc");
    }

    (void)memset(ap->child, 0, sizeof(alcove_child_t) * ap->fdsetsize);

    fds = calloc(sizeof(struct pollfd), ap->maxfd);
    if (!fds)
        erl_err_sys("calloc");

    for ( ; ; ) {
        long maxfd = sysconf(_SC_OPEN_MAX);
        int i = 0;
        nfds_t nfds = STDIN_FILENO;

        if (alcove_handle_signal(ap) < 0)
            erl_err_sys("alcove_handle_signal");

        if (ap->maxfd < maxfd) {
            ap->maxfd = maxfd;
            fds = realloc(fds, sizeof(struct pollfd) * maxfd);
            if (!fds)
                erl_err_sys("realloc");
            (void)memset(fds, 0, sizeof(struct pollfd) * maxfd);
        }

        for (i = 0; i < maxfd; i++) {
            fds[i].fd = -1;
            fds[i].revents = 0;
        }

        fds[STDIN_FILENO].fd = STDIN_FILENO;
        fds[STDIN_FILENO].events = POLLIN;

        (void)pid_foreach(ap, 0, fds, &nfds, pid_not_equal, set_pid);

        if (poll(fds, nfds+1, -1) < 0) {
            switch (errno) {
                case EINTR:
                    continue;
                default:
                    break;
            }
        }

        if (fds[STDIN_FILENO].revents & (POLLIN|POLLERR|POLLHUP|POLLNVAL)) {
            switch (alcove_stdin(ap)) {
                case -1:
                    erl_err_sys("alcove_stdin");
                case 1:
                    /* EOF */
                    free(fds);
                    return;
                case 0:
                    break;
            }
        }

        pid_foreach(ap, 0, fds, NULL, pid_not_equal, read_from_pid);

        if (ap->verbose > 1)
            alcove_stats(ap);
    }
}

    static int
alcove_stdin(alcove_state_t *ap)
{
    u_int16_t buflen = 0;
    u_int16_t type = 0;
    pid_t pid = 0;
    unsigned char buf[MAXMSGLEN] = {0};
    unsigned char *pbuf = buf;

    errno = 0;

    /*
     * Call:
     *  |length:2|call:2|command:2|arg:...|
     *
     * Stdin:
     *  |length:2|stdin:2|pid:4|data:...|
     *
     */

    /* total length, not including length header */
    if (alcove_get_uint16(STDIN_FILENO, &buflen) != sizeof(buflen)) {
        if (errno == 0)
            return 1;

        return -1;
    }

    if (alcove_read(STDIN_FILENO, buf, buflen) != buflen)
        return -1;

    type = get_int16(pbuf);
    pbuf += 2;
    buflen -= 2;

    switch (type) {
        case ALCOVE_MSG_CALL:
            if (alcove_msg_call(ap, pbuf, buflen) < 0)
                return -1;

            return 0;

        case ALCOVE_MSG_STDIN:
            if (buflen < sizeof(pid))
                return -1;

            pid = get_int32(pbuf);
            pbuf += 4;
            buflen -= 4;

            switch (pid_foreach(ap, pid, pbuf, &buflen, pid_equal, write_to_pid)) {
                case -2:
                    return -1;
                case -1:
                    /* XXX badarg */
                case 0:
                    return 0;
            }

        default:
            return -1;
    }
}

    static ssize_t
alcove_msg_call(alcove_state_t *ap, unsigned char *buf, u_int16_t buflen)
{
    u_int16_t call = 0;
    ETERM *arg = NULL;
    ETERM *reply = NULL;
    ssize_t rv = -1;

    if (buflen <= sizeof(call))
        return -1;

    call = get_int16(buf);
    buf += 2;

    arg = erl_decode(buf);
    if (!arg)
        goto DONE;

    reply = alcove_call(ap, call, arg);
    if (!reply)
        goto DONE;

    rv = alcove_call_reply(ALCOVE_MSG_CALL, reply);

DONE:
    erl_free_compound(arg);
    erl_free_compound(reply);

    return rv;
}

    static ssize_t
alcove_child_stdio(int fdin, u_int16_t depth, alcove_child_t *c, u_int16_t type)
{
    struct iovec iov[4];

    ssize_t n = 0;
    u_int16_t len = 0;
    size_t read_len = sizeof(len);
    unsigned char buf[MAXMSGLEN] = {0};
    pid_t pidbe = htonl(c->pid);
    u_int16_t typebe = htons(type);

    /* If the child has called exec(), treat the data as a stream.
     *
     * Otherwise, read in the length header and do an exact read.
     */
    if ( (c->fdctl == ALCOVE_CHILD_EXEC)
            || (type == ALCOVE_MSG_STDERR))
        read_len = ALCOVE_MSGLEN(depth, sizeof(buf));

    errno = 0;
    n = read(fdin, buf, read_len);

    if (n <= 0) {
        if (errno == 0)
            return 0;

        return -1;
    }

    if ( (c->fdctl != ALCOVE_CHILD_EXEC)
            && (type != ALCOVE_MSG_STDERR)) {
        if (n < 2)
            return -1;

        n = get_int16(buf);

        if (n > sizeof(buf) - 2)
            return -1;

        if (alcove_read(fdin, buf+2, n) != n)
            return -1;

        n += 2;
    }

    len = htons(sizeof(typebe) + sizeof(pidbe) + n);

    iov[0].iov_base = &len;
    iov[0].iov_len = sizeof(len);
    iov[1].iov_base = &typebe;
    iov[1].iov_len = sizeof(typebe);
    iov[2].iov_base = &pidbe;
    iov[2].iov_len = sizeof(pidbe);
    iov[3].iov_base = buf;
    iov[3].iov_len = n;

    return writev(STDOUT_FILENO, iov, sizeof(iov)/sizeof(iov[0]));
}

    static ssize_t
alcove_call_reply(u_int16_t type, ETERM *t)
{
    struct iovec iov[3];

    u_int16_t len = 0;
    u_int16_t typebe = htons(type);
    unsigned char buf[MAXMSGLEN] = {0};
    int buflen = 0;

    buflen = erl_term_len(t);
    if (buflen < 0 || buflen > sizeof(buf))
        return -1;

    if (erl_encode(t, buf) < 1)
        return -1;

    len = htons(sizeof(typebe) + buflen);

    iov[0].iov_base = &len;
    iov[0].iov_len = sizeof(len);
    iov[1].iov_base = &typebe;
    iov[1].iov_len = sizeof(typebe);
    iov[2].iov_base = buf;
    iov[2].iov_len = buflen;

    return writev(STDOUT_FILENO, iov, sizeof(iov)/sizeof(iov[0]));
}

    static ssize_t
alcove_call_fake_reply(pid_t pid, u_int16_t type, ETERM *t)
{
    struct iovec iov[6];

    u_int16_t total_len = 0;
    u_int16_t hdr_type = htons(ALCOVE_MSG_PROXY);
    pid_t pidbe = htonl(pid);

    u_int16_t call_len = 0;
    u_int16_t call_type = htons(type);
    unsigned char buf[MAXMSGLEN] = {0};
    int buflen = 0;

    ssize_t n = -1;

    buflen = erl_term_len(t);
    if (buflen < 0 || buflen > sizeof(buf))
        return -1;

    if (erl_encode(t, buf) < 1)
        return -1;

    total_len = htons(sizeof(hdr_type) + sizeof(pid) + sizeof(call_len) +
            sizeof(call_type) + buflen);
    call_len = htons(sizeof(call_type) + buflen);

    /* Proxy header */
    iov[0].iov_base = &total_len;
    iov[0].iov_len = sizeof(total_len);
    iov[1].iov_base = &hdr_type;
    iov[1].iov_len = sizeof(hdr_type);
    iov[2].iov_base = &pidbe;
    iov[2].iov_len = sizeof(pidbe);

    /* Call */
    iov[3].iov_base = &call_len;
    iov[3].iov_len = sizeof(call_len);
    iov[4].iov_base = &call_type;
    iov[4].iov_len = sizeof(call_type);
    iov[5].iov_base = buf;
    iov[5].iov_len = buflen;

    n = writev(STDOUT_FILENO, iov, sizeof(iov)/sizeof(iov[0]));

    erl_free_compound(t);

    return n;
}

    static int
alcove_get_uint16(int fd, u_int16_t *val)
{
    u_int16_t buf = 0;
    ssize_t n = 0;

    n = alcove_read(fd, &buf, sizeof(buf));

    if (n != sizeof(buf))
        return n;

    *val = ntohs(buf);
    return n;
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

    int
pid_foreach(alcove_state_t *ap, pid_t pid, void *arg1, void *arg2,
        int (*comp)(pid_t, pid_t),
        int (*fp)(alcove_state_t *ap, alcove_child_t *, void *, void *))
{
    int i = 0;
    int rv = 0;

    for (i = 0; i < ap->fdsetsize; i++) {
        if ((*comp)(ap->child[i].pid, pid) == 0)
            continue;

        rv = (*fp)(ap, &(ap->child[i]), arg1, arg2);

        if (rv <= 0)
            return rv;
    }

    return 1;
}

    int
pid_equal(pid_t p1, pid_t p2)
{
    return (p1 == p2);
}

    int
pid_not_equal(pid_t p1, pid_t p2)
{
    return (p1 != p2);
}

    static int
exited_pid(alcove_state_t *ap, alcove_child_t *c, void *arg1, void *arg2)
{
    int *status = arg1;
    ETERM *t = NULL;

    c->exited = 1;
    (void)close(c->fdin);
    c->fdin = -1;

    if (WIFEXITED(*status) && (ap->opt & alcove_opt_exit_status)) {
        t = alcove_tuple2(
                erl_mk_atom("exit_status"),
                erl_mk_int(WEXITSTATUS(*status))
                );

        if (alcove_call_fake_reply(c->pid, ALCOVE_MSG_EVENT, t))
            return -1;
    }

    if (WIFSIGNALED(*status) && (ap->opt & alcove_opt_termsig)) {
        t = alcove_tuple2(
                erl_mk_atom("termsig"),
                erl_mk_int(WTERMSIG(*status))
                );

        if (alcove_call_fake_reply(c->pid, ALCOVE_MSG_EVENT, t) < 0)
            return -1;
    }

    return 0;
}

    static int
set_pid(alcove_state_t *ap, alcove_child_t *c, void *arg1, void *arg2)
{
    struct pollfd *fds = arg1;
    nfds_t *nfds = arg2;

    if (c->fdctl > -1) {
        fds[c->fdctl].fd = c->fdctl;
        fds[c->fdctl].events = POLLIN;
        *nfds = MAX(*nfds, c->fdctl);
    }

    if (c->fdout > -1) {
        fds[c->fdout].fd = c->fdout;
        fds[c->fdout].events = POLLIN;
        *nfds = MAX(*nfds, c->fdout);
    }

    if (c->fderr > -1) {
        fds[c->fderr].fd = c->fderr;
        fds[c->fderr].events = POLLIN;
        *nfds = MAX(*nfds, c->fderr);
    }

    if (c->exited && c->fdout == -1 && c->fderr == -1 && c->fdctl < 0) {
        c->pid = 0;
        c->exited = 0;
    }

    return 1;
}

    static int
write_to_pid(alcove_state_t *ap, alcove_child_t *c, void *arg1, void *arg2)
{
    char *buf = arg1;
    u_int16_t *buflen = arg2;

    if (c->fdin == -1)
        return -1;

    if (write(c->fdin, buf, *buflen) != *buflen)
        return -2;

    return 0;
}

    static int
read_from_pid(alcove_state_t *ap, alcove_child_t *c, void *arg1, void *arg2)
{
    struct pollfd *fds = arg1;

    if (c->fdctl > -1 && (fds[c->fdctl].revents & (POLLIN|POLLERR|POLLHUP|POLLNVAL))) {
        unsigned char buf;
        ssize_t n;

        n = read(c->fdctl, &buf, sizeof(buf));
        (void)close(c->fdctl);
        c->fdctl = -1;

        if (n == 0) {
            c->fdctl = ALCOVE_CHILD_EXEC;
            if (alcove_call_fake_reply(c->pid, ALCOVE_MSG_CALL,
                        erl_mk_atom("ok")) < 0)
                return -1;
        }
    }

    if (c->fdout > -1 && (fds[c->fdout].revents & (POLLIN|POLLERR|POLLHUP|POLLNVAL))) {
        switch (alcove_child_stdio(c->fdout, ap->depth,
                    c, ALCOVE_MSG_TYPE(c))) {
            case -1:
            case 0:
                (void)close(c->fdout);
                c->fdout = -1;
                break;
            default:
                break;
        }
    }

    if (c->fderr > -1 && (fds[c->fderr].revents & (POLLIN|POLLERR|POLLHUP|POLLNVAL))) {
        switch (alcove_child_stdio(c->fderr, ap->depth,
                    c, ALCOVE_MSG_STDERR)) {
            case -1:
            case 0:
                (void)close(c->fderr);
                c->fderr = -1;
                break;
            default:
                break;
        }
    }

    return 1;
}

    static int
alcove_handle_signal(alcove_state_t *ap) {
    int signum = 0;
    ETERM *reply = NULL;
    int status = 0;
    int rv = -1;

    if (!sigcaught)
        return 0;

    for (signum = 0; signum < sizeof(sigcaught) * 8; signum++) {
        if (!(sigcaught & (1 << signum)))
            continue;

        sigcaught &= ~(1 << signum);

        if (signum == SIGCHLD) {
            pid_t pid = 0;

            for ( ; ; ) {
                errno = 0;
                pid = waitpid(-1, &status, WNOHANG);

                if (errno == ECHILD || pid == 0)
                    break;

                if (pid < 0)
                    return -1;

                (void)pid_foreach(ap, pid, &status, NULL,
                        pid_equal, exited_pid);
            }

            if (!(ap->opt & alcove_opt_sigchld))
                continue;
        }

        reply = alcove_tuple2(
                erl_mk_atom("signal"),
                erl_mk_int(signum)
                );

        if (alcove_call_reply(ALCOVE_MSG_EVENT, reply) < 0) {
            erl_free_compound(reply);
            goto DONE;
        }

        erl_free_compound(reply);
    }

    rv = 0;

DONE:
    return rv;
}


    static void
alcove_stats(alcove_state_t *ap)
{
    unsigned long allocated = 0;
    unsigned long freed = 0;

    erl_eterm_statistics(&allocated, &freed);
    VERBOSE(2, "allocated=%ld, freed=%ld", allocated, freed);
    erl_eterm_release();
}

    static void
usage(alcove_state_t *ap)
{
    (void)fprintf(stderr, "%s %s\n",
            __progname, ALCOVE_VERSION);
    (void)fprintf(stderr,
            "usage: %s <options>\n"
            "   -m <num>        max children\n"
            "   -M <num>        max fork depth\n"
            "   -e <0|1>        child exit status\n"
            "   -s <0|1>        sigchld\n"
            "   -S <0|1>        child termination signal\n"
            "   -v              verbose mode\n",
            __progname
            );

    exit (EXIT_FAILURE);
}
