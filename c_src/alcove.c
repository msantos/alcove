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
    ALCOVE_MSG_CTL,
};

#define ALCOVE_CHILD_EXEC -2

#define ALCOVE_MSG_TYPE(s) \
    ((s->fdctl == ALCOVE_CHILD_EXEC) ? ALCOVE_MSG_STDOUT : ALCOVE_MSG_PROXY)

#define ALCOVE_IOVEC_COUNT(_array) (sizeof(_array)/sizeof(_array[0]))

#ifdef __linux__
#pragma message "Support for namespaces using clone(2) enabled"
#else
#pragma message "Support for namespaces using clone(2) disabled"
#endif

static int alcove_signal_init();
static int alcove_rlimit_init();
static int alcove_fd_init();
int alcove_fdmove(int fd, int dst);

static int alcove_stdin(alcove_state_t *ap);
static ssize_t alcove_msg_call(alcove_state_t *ap, unsigned char *buf,
        u_int16_t buflen);

static size_t alcove_proxy_hdr(unsigned char *hdr, size_t hdrlen,
        u_int16_t type, pid_t pid, size_t buflen);
static size_t alcove_call_hdr(unsigned char *hdr, size_t hdrlen,
        u_int16_t type, size_t buflen);

static ssize_t alcove_child_stdio(int fdin, u_int16_t depth,
        alcove_child_t *c, u_int16_t type);
static ssize_t alcove_call_reply(u_int16_t, char *, size_t);
static ssize_t alcove_call_fake_reply(pid_t pid, u_int16_t type,
        char *, size_t);

static int alcove_get_uint16(int fd, u_int16_t *val);
static ssize_t alcove_read(int, void *, ssize_t);
static ssize_t alcove_write(int fd, struct iovec *iov, int count);

static int exited_pid(alcove_state_t *ap, alcove_child_t *c,
        void *arg1, void *arg2);
static int set_pid(alcove_state_t *ap, alcove_child_t *c,
        void *arg1, void *arg2);
static int write_to_pid(alcove_state_t *ap, alcove_child_t *c,
        void *arg1, void *arg2);
static int read_from_pid(alcove_state_t *ap, alcove_child_t *c,
        void *arg1, void *arg2);

static int alcove_handle_signal(alcove_state_t *ap);

static void usage(alcove_state_t *);

extern char *__progname;

    void
sighandler(int sig)
{
    if (write(ALCOVE_SIGWRITE_FILENO, &sig, sizeof(sig)) != sizeof(sig))
        (void)close(ALCOVE_SIGWRITE_FILENO);
}

    int
main(int argc, char *argv[])
{
    alcove_state_t *ap = NULL;
    int ch = 0;

    ap = calloc(1, sizeof(alcove_state_t));
    if (ap == NULL)
        err(EXIT_FAILURE, "calloc");

    ALCOVE_SETOPT(ap, alcove_opt_termsig, 1);
    ALCOVE_SETOPT(ap, alcove_opt_exit_status, 1);

    ap->maxfd = sysconf(_SC_OPEN_MAX);
    ap->maxchild = ap->maxfd / ALCOVE_MAXFILENO - ALCOVE_MAXFILENO;
    ap->maxforkdepth = MAXFORKDEPTH;

    if (alcove_signal_init() < 0)
        err(EXIT_FAILURE, "alcove_signal_init");

    if (alcove_rlimit_init() < 0)
        err(EXIT_FAILURE, "alcove_rlimit_init");

    while ( (ch = getopt(argc, argv, "m:hv")) != -1) {
        switch (ch) {
            case 'm':
                ap->maxchild = (u_int16_t)atoi(optarg);
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
    if (ap->child == NULL)
        err(EXIT_FAILURE, "calloc");

    if (alcove_fd_init() < 0)
        err(EXIT_FAILURE, "alcove_fd_init");

    alcove_event_loop(ap);
    exit(0);
}

    static int
alcove_signal_init()
{
    struct sigaction act;
    int sig = 0;

    (void)memset(&act, 0, sizeof(act));
    act.sa_handler = SIG_DFL;
    (void)sigfillset(&act.sa_mask);

    for (sig = 1; sig < NSIG; sig++) {
        if (sigaction(sig, &act, NULL) < 0) {
            if (errno == EINVAL)
                continue;

            return -1;
        }
    }

    act.sa_handler = sighandler;

    if (sigaction(SIGCHLD, &act, NULL) < 0)
        return -1;

    return 0;
}

    static int
alcove_rlimit_init()
{
    struct rlimit stack_size = {0};

    if (getrlimit(RLIMIT_STACK, &stack_size) < 0)
        return -1;

    /* Reset an unlimited stack size to a default value. The default is
     * set to 8Mb, the default for linux (_STK_LIM in linux/resources.h).
     *
     * The current value of RLIMIT_STACK is used for allocating the
     * stack of cloned processes.
     *
     */

    if (stack_size.rlim_cur == RLIM_INFINITY) {
        stack_size.rlim_cur = 8 * 1024 * 1024;
        if (setrlimit(RLIMIT_STACK, &stack_size) < 0)
            return -1;
    }

    return 0;
}

    static int
alcove_fd_init()
{
    int sigpipe[2] = {0};
    int fdctl = 0;

    if (alcove_fdmove(ALCOVE_SIGREAD_FILENO, 8) < 0)
        return -1;

    if (alcove_fdmove(ALCOVE_SIGWRITE_FILENO, 8) < 0)
        return -1;

    if (alcove_fdmove(ALCOVE_FDCTL_FILENO, 8) < 0)
        return -1;

    if (pipe(sigpipe) < 0)
        return -1;

    /* XXX fd's will overlap */
    if (sigpipe[0] != ALCOVE_SIGREAD_FILENO) {
        if (dup2(sigpipe[0], ALCOVE_SIGREAD_FILENO) < 0)
            return -1;
        if (close(sigpipe[0]) < 0)
            return -1;
    }

    if (sigpipe[1] != ALCOVE_SIGWRITE_FILENO) {
        if (dup2(sigpipe[1], ALCOVE_SIGWRITE_FILENO) < 0)
            return -1;
        if (close(sigpipe[1]) < 0)
            return -1;
    }

    if ( (alcove_setfd(ALCOVE_SIGREAD_FILENO, FD_CLOEXEC|O_NONBLOCK) < 0)
            || (alcove_setfd(ALCOVE_SIGWRITE_FILENO, FD_CLOEXEC|O_NONBLOCK) < 0))
        return -1;

    /* Unlike the child processes, the port does not use a control fd.
     * An fd is acquired and leaked here to reserve it.
     *
     * The fd may have been opened by another program. For example,
     * valgrind will use the first available fd for the log file.
     */
    fdctl = open("/dev/null", O_RDWR|O_CLOEXEC);
    if (fdctl < 0)
        return -1;

    if (fdctl != ALCOVE_FDCTL_FILENO) {
        if (dup2(fdctl, ALCOVE_FDCTL_FILENO) < 0)
            return -1;
        if (close(fdctl) < 0)
            return -1;
    }

    return 0;
}

    void
alcove_event_loop(alcove_state_t *ap)
{
    struct pollfd *fds = NULL;

    if (ap->fdsetsize != ap->maxchild) {
        /* the array may be shrinking */
        (void)memset(ap->child, 0, sizeof(alcove_child_t) * ap->fdsetsize);

        ap->fdsetsize = ap->maxchild;
        ap->child = realloc(ap->child, sizeof(alcove_child_t) * ap->fdsetsize);

        if (ap->child == NULL)
            err(errno, "realloc");
    }

    (void)memset(ap->child, 0, sizeof(alcove_child_t) * ap->fdsetsize);

    fds = calloc(sizeof(struct pollfd), ap->maxfd);
    if (fds == NULL)
        err(errno, "calloc");

    for ( ; ; ) {
        long maxfd = sysconf(_SC_OPEN_MAX);
        int i = 0;

        if (ap->maxfd < maxfd) {
            ap->maxfd = maxfd;
            fds = realloc(fds, sizeof(struct pollfd) * maxfd);
            if (fds == NULL)
                err(errno, "realloc");
            (void)memset(fds, 0, sizeof(struct pollfd) * maxfd);
        }

        for (i = 0; i < ap->maxfd; i++) {
            fds[i].fd = -1;
            fds[i].revents = 0;
        }

        fds[STDIN_FILENO].fd = STDIN_FILENO;
        fds[STDIN_FILENO].events = POLLIN;

        fds[ALCOVE_SIGREAD_FILENO].fd = ALCOVE_SIGREAD_FILENO;
        fds[ALCOVE_SIGREAD_FILENO].events = POLLIN;

        (void)pid_foreach(ap, 0, fds, NULL, pid_not_equal, set_pid);

#if defined(__linux__) || defined(__sunos__) || defined(__OpenBSD__)
        if (poll(fds, maxfd, -1) < 0) {
#else
        if (poll(fds, ap->maxfd, -1) < 0) {
#endif
            switch (errno) {
                case EINTR:
                    continue;
                default:
                    err(errno, "poll");
            }
        }

        if (fds[STDIN_FILENO].revents & (POLLIN|POLLERR|POLLHUP|POLLNVAL)) {
            switch (alcove_stdin(ap)) {
                case 0:
                    break;
                case 1:
                    /* EOF */
                    free(fds);
                    return;
                case -1:
                default:
                    err(errno, "alcove_stdin");
            }
        }

        if (fds[ALCOVE_SIGREAD_FILENO].revents & (POLLIN|POLLERR|POLLHUP|POLLNVAL)) {
            if (alcove_handle_signal(ap) < 0)
                err(errno, "alcove_handle_signal");
        }

        (void)pid_foreach(ap, 0, fds, NULL, pid_not_equal, read_from_pid);
    }
}

    static int
alcove_stdin(alcove_state_t *ap)
{
    u_int16_t type = 0;
    pid_t pid = 0;
    unsigned char msg[MAXMSGLEN] = {0};
    unsigned char *buf = msg;
    u_int16_t buflen = 0;

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

    type = get_int16(buf);
    buf += 2;
    buflen -= 2;

    switch (type) {
        case ALCOVE_MSG_CALL:
            if (alcove_msg_call(ap, buf, buflen) < 0)
                return -1;

            return 0;

        case ALCOVE_MSG_STDIN:
            if (buflen < sizeof(pid))
                return -1;

            pid = get_int32(buf);
            buf += 4;
            buflen -= 4;

            if ( (pid <= 0) || (pid_foreach(ap, pid, buf, &buflen, pid_equal,
                            write_to_pid) == 1)) {
                int tlen = 0;
                char t[MAXMSGLEN] = {0};
                tlen = alcove_mk_atom(t, sizeof(t), "badpid");
                if (alcove_call_fake_reply(pid, ALCOVE_MSG_CTL, t, tlen) < 0)
                    return -1;
            }

            return 0;

        default:
            return -1;
    }
}

    static ssize_t
alcove_msg_call(alcove_state_t *ap, unsigned char *buf, u_int16_t buflen)
{
    u_int16_t call = 0;
    char reply[MAXMSGLEN] = {0};
    ssize_t rlen = 0;

    if (buflen <= sizeof(call))
        return -1;

    call = get_int16(buf);
    buf += 2;

    rlen = alcove_call(ap, call, (const char *)buf, buflen,
            reply, sizeof(reply));

    /* Must crash on error. The port may have allocated memory or
     * performed some other destructive action.
     */
    if (rlen < 0)
        return -1;

    return alcove_call_reply(ALCOVE_MSG_CALL, reply, rlen);
}

    static size_t
alcove_proxy_hdr(unsigned char *hdr, size_t hdrlen, u_int16_t type,
        pid_t pid, size_t buflen)
{
    u_int16_t len = 0;

    put_int16(sizeof(type) + sizeof(pid) + buflen, hdr); len = 2;
    put_int16(type, hdr+len); len += 2;
    put_int32(pid, hdr+len); len += 4;

    return len;
}

    static size_t
alcove_call_hdr(unsigned char *hdr, size_t hdrlen, u_int16_t type,
        size_t buflen)
{
    u_int16_t len = 0;

    put_int16(sizeof(type) + buflen, hdr); len = 2;
    put_int16(type, hdr+len); len += 2;

    return len;
}

    static ssize_t
alcove_child_stdio(int fdin, u_int16_t depth, alcove_child_t *c,
        u_int16_t type)
{
    struct iovec iov[2];

    ssize_t n = 0;
    unsigned char buf[MAXMSGLEN] = {0};
    unsigned char hdr[MAXHDRLEN] = {0};
    u_int16_t hdrlen = 0;
    size_t read_len = sizeof(hdrlen);

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


    hdrlen = alcove_proxy_hdr(hdr, sizeof(hdr), type, c->pid, n);

    iov[0].iov_base = hdr;
    iov[0].iov_len = hdrlen;
    iov[1].iov_base = buf;
    iov[1].iov_len = n;

    return alcove_write(STDOUT_FILENO, iov, ALCOVE_IOVEC_COUNT(iov));
}

    static ssize_t
alcove_call_reply(u_int16_t type, char *buf, size_t len)
{
    struct iovec iov[2];

    unsigned char hdr[MAXHDRLEN] = {0};
    u_int16_t hdrlen = 0;

    hdrlen = alcove_call_hdr(hdr, sizeof(hdr), type, len);

    iov[0].iov_base = hdr;
    iov[0].iov_len = hdrlen;
    iov[1].iov_base = buf;
    iov[1].iov_len = len;

    return alcove_write(STDOUT_FILENO, iov, ALCOVE_IOVEC_COUNT(iov));
}

    static ssize_t
alcove_call_fake_reply(pid_t pid, u_int16_t type, char *buf, size_t len)
{
    struct iovec iov[3];

    unsigned char proxyhdr[MAXHDRLEN] = {0};
    u_int16_t proxyhdrlen = 0;

    unsigned char callhdr[MAXHDRLEN] = {0};
    u_int16_t callhdrlen = 0;

    ssize_t n = -1;

    callhdrlen = alcove_call_hdr(callhdr, sizeof(callhdr), type, len);

    proxyhdrlen = alcove_proxy_hdr(proxyhdr, sizeof(proxyhdr),
            ALCOVE_MSG_PROXY, pid, callhdrlen + len);

    iov[0].iov_base = proxyhdr;
    iov[0].iov_len = proxyhdrlen;
    iov[1].iov_base = callhdr;
    iov[1].iov_len = callhdrlen;
    iov[2].iov_base = buf;
    iov[2].iov_len = len;

    n = alcove_write(STDOUT_FILENO, iov, ALCOVE_IOVEC_COUNT(iov));

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
            return i;
        got += i;
    } while (got < len);

    return len;
}

    static ssize_t
alcove_write(int fd, struct iovec *iov, int count)
{
    ssize_t written = 0;
    ssize_t n = 0;
    int offset = 0;

    do {
        iov[offset].iov_base = (char *)iov[offset].iov_base + written;
        iov[offset].iov_len -= written;

        written = writev(fd, iov+offset, count-offset);
        if (written <= 0)
            return written;

        n += written;

        for ( ; offset < count && written >= iov[offset].iov_len; offset++)
            written -= iov[offset].iov_len;

    } while (offset < count);

    return n;
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
    return p1 == p2;
}

    int
pid_not_equal(pid_t p1, pid_t p2)
{
    return p1 != p2;
}

    static int
exited_pid(alcove_state_t *ap, alcove_child_t *c, void *arg1, void *arg2)
{
    int *status = arg1;
    int index = 0;
    char t[MAXMSGLEN] = {0};

    if ( (c->fdin >= 0) && (ap->opt & alcove_opt_stdin_closed)) {
        index = alcove_mk_atom(t, sizeof(t), "stdin_closed");
        if (alcove_call_fake_reply(c->pid, ALCOVE_MSG_CTL, t, index) < 0)
            return -1;
    }

    c->exited = 1 << 8;
    (void)close(c->fdin);
    c->fdin = -1;

    if (WIFEXITED(*status)) {
        c->exited |= WEXITSTATUS(*status);

        if (ap->opt & alcove_opt_exit_status) {
            ALCOVE_TUPLE2(t, &index,
                    "exit_status",
                    ei_encode_long(t, &index, WEXITSTATUS(*status))
                    );

            if (alcove_call_fake_reply(c->pid, ALCOVE_MSG_EVENT, t, index))
                return -1;
        }
    }

    if (WIFSIGNALED(*status)) {
        c->termsig = WTERMSIG(*status);

        if (ap->opt & alcove_opt_termsig) {
            ALCOVE_TUPLE2(t, &index,
                "termsig",
                alcove_signal_name(t, sizeof(t), &index, c->termsig)
            );

            if (alcove_call_fake_reply(c->pid, ALCOVE_MSG_EVENT, t, index) < 0)
                return -1;
        }
    }

    return 0;
}

    static int
set_pid(alcove_state_t *ap, alcove_child_t *c, void *arg1, void *arg2)
{
    struct pollfd *fds = arg1;

    if (c->fdctl > -1) {
        fds[c->fdctl].fd = c->fdctl;
        fds[c->fdctl].events = POLLIN;
    }

    if (c->fdout > -1) {
        fds[c->fdout].fd = c->fdout;
        fds[c->fdout].events = POLLIN;
    }

    if (c->fderr > -1) {
        fds[c->fderr].fd = c->fderr;
        fds[c->fderr].events = POLLIN;
    }

    if (c->exited && c->fdout == -1 && c->fderr == -1 && c->fdctl < 0) {
        c->pid = 0;
        c->exited = 0;
        c->termsig = 0;
    }

    return 1;
}

    static int
write_to_pid(alcove_state_t *ap, alcove_child_t *c, void *arg1, void *arg2)
{
    char *buf = arg1;
    u_int16_t *buflen = arg2;
    ssize_t n = 0;
    ssize_t written = 0;

    if (c->fdin == -1)
        return -2;

    do {
        n = write(c->fdin, buf + written, *buflen - written);

        if (n <= 0)
            return n;

        written += n;
    } while (written < *buflen);

    return 0;
}

    static int
read_from_pid(alcove_state_t *ap, alcove_child_t *c, void *arg1, void *arg2)
{
    struct pollfd *fds = arg1;
    int len = 0;
    char t[MAXMSGLEN] = {0};

    if (c->fdctl > -1 &&
            (fds[c->fdctl].revents & (POLLIN|POLLERR|POLLHUP|POLLNVAL))) {
        unsigned char buf;
        ssize_t n;

        n = read(c->fdctl, &buf, sizeof(buf));
        (void)close(c->fdctl);
        c->fdctl = -1;

        if (n == 0) {
            c->fdctl = ALCOVE_CHILD_EXEC;
            len = alcove_mk_atom(t, sizeof(t), "fdctl_closed");

            if (alcove_call_fake_reply(c->pid, ALCOVE_MSG_CTL, t, len) < 0)
                return -1;
        }
    }

    if (c->fdout > -1 &&
            (fds[c->fdout].revents & (POLLIN|POLLERR|POLLHUP|POLLNVAL))) {
        switch (alcove_child_stdio(c->fdout, ap->depth,
                    c, ALCOVE_MSG_TYPE(c))) {
            case 0:
                if (ap->opt & alcove_opt_stdout_closed) {
                    len = alcove_mk_atom(t, sizeof(t), "stdout_closed");
                    if (alcove_call_fake_reply(c->pid, ALCOVE_MSG_CTL,
                                t, len) < 0)
                        return -1;
                }
                /* fall through */
            case -1:
                (void)close(c->fdout);
                c->fdout = -1;
                break;
            default:
                break;
        }
    }

    if (c->fderr > -1 &&
            (fds[c->fderr].revents & (POLLIN|POLLERR|POLLHUP|POLLNVAL))) {
        switch (alcove_child_stdio(c->fderr, ap->depth,
                    c, ALCOVE_MSG_STDERR)) {
            case 0:
                if (ap->opt & alcove_opt_stderr_closed) {
                    len = alcove_mk_atom(t, sizeof(t), "stderr_closed");
                    if (alcove_call_fake_reply(c->pid, ALCOVE_MSG_CTL,
                                t, len) < 0)
                        return -1;
                }
                /* fall through */
            case -1:
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
    int index = 0;
    char reply[MAXMSGLEN] = {0};
    int signum = 0;
    int status = 0;
    ssize_t n = 0;

    n = read(ALCOVE_SIGREAD_FILENO, &signum, sizeof(signum));

    if (n < 0) {
        if (errno == EAGAIN || errno == EINTR)
            return 0;

        return -1;
    }
    else if (n == 0 || n != sizeof(signum))
        return -1;

    if (signum == SIGCHLD) {
        pid_t pid = 0;

        errno = 0;
        pid = waitpid(-1, &status, WNOHANG);

        if (errno == ECHILD || pid == 0)
            return -1;

        if (pid < 0)
            return -1;

        (void)pid_foreach(ap, pid, &status, NULL,
                pid_equal, exited_pid);

        if ((ap->opt & alcove_opt_sigchld) == 0)
            return 0;
    }

    ALCOVE_TUPLE2(reply, &index,
        "signal",
        alcove_signal_name(reply, sizeof(reply), &index, signum)
    );

    if (alcove_call_reply(ALCOVE_MSG_EVENT, reply, index) < 0)
        return -1;

    return 0;
}

    int
alcove_fdmove(int fd, int dst)
{
    int flags = 0;

    flags = fcntl(fd, F_GETFD);

    if (flags < 0)
        return 0;

    if (fcntl(fd, F_DUPFD, dst) < 0)
        return -1;

    /* According to fcntl(2) on FreeBSD, the close-on-exec flag is reset
     * by F_DUPFD:
     *
     * The close-on-exec flag FD_CLOEXEC associated
     * with the new file descriptor is cleared, so the
     * file descriptor is to remain open across
     * execve(2) system calls.
     *
     * Restore the flag if it was set:
     *
     */
    return fcntl(dst, F_SETFD, flags);
}

    int
alcove_setfd(int fd, int flag)
{
    int flags = 0;

    flags = fcntl(fd, F_GETFD, 0);
    if (flags < 0)
        return -1;

    return fcntl(fd, F_SETFD, flags | flag);
}

    static void
usage(alcove_state_t *ap)
{
    (void)fprintf(stderr, "%s %s\n",
            __progname, ALCOVE_VERSION);
    (void)fprintf(stderr,
            "usage: %s <options>\n"
            "   -m <num>        max children\n"
            "   -v              verbose mode\n",
            __progname
            );

    exit (EXIT_FAILURE);
}
