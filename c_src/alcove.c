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

#include <signal.h>
#include <sys/select.h>
#include <sys/wait.h>

#define ALCOVE_MSG_STDIN    0
#define ALCOVE_MSG_STDOUT   (htons(1))
#define ALCOVE_MSG_STDERR   (htons(2))
#define ALCOVE_MSG_PROXY    (htons(3))
#define ALCOVE_MSG_CALL     (htons(4))
#define ALCOVE_MSG_EVENT    (htons(5))

#define ALCOVE_CHILD_EXEC -2

#define ALCOVE_MSG_TYPE(s) \
    ((s->fdctl == ALCOVE_CHILD_EXEC) ? ALCOVE_MSG_STDOUT : ALCOVE_MSG_PROXY)

#ifdef __linux__
#pragma message "Support for namespaces using clone(2) enabled"
#else
#pragma message "Support for namespaces using clone(2) disabled"
#endif

typedef struct {
    u_int16_t len;
    u_int16_t type;
    pid_t pid;
    unsigned char buf[MAXMSGLEN];
} alcove_msg_stdio_t;

typedef struct {
    u_int16_t len;
    u_int16_t type;
    unsigned char buf[MAXMSGLEN];
} alcove_msg_call_t;

static int alcove_stdin(alcove_state_t *ap);
static ssize_t alcove_msg_call(alcove_state_t *ap, int fd, u_int16_t len);
static alcove_msg_t *alcove_msg_read(int fd, u_int16_t len);

static ssize_t alcove_child_stdio(int fdin, u_int16_t depth, alcove_child_t *c, u_int16_t type);
static ssize_t alcove_send(u_int16_t, ETERM *);
static ssize_t alcove_call_stdio(pid_t pid, u_int16_t type, ETERM *t);
static ssize_t alcove_write(void *data, size_t len);
static ssize_t alcove_read(int, void *, ssize_t);

static alcove_msg_stdio_t * alcove_alloc_hdr_stdio(pid_t pid, u_int16_t type,
        void *buf, size_t *len);
static alcove_msg_call_t * alcove_alloc_hdr_call(u_int16_t type, ETERM *t,
        size_t *len);

static int exited_pid(alcove_child_t *c, void *arg1, void *arg2);
static int set_pid(alcove_child_t *c, void *arg1, void *arg2);
static int write_to_pid(alcove_child_t *c, void *arg1, void *arg2);
static int read_from_pid(alcove_child_t *c, void *arg1, void *arg2);

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

    ap = calloc(1, sizeof(alcove_state_t));
    if (!ap)
        erl_err_sys("calloc");

    act.sa_handler = sighandler;
    if (sigaction(SIGCHLD, &act, NULL) < 0)
        erl_err_sys("sigaction");

    ap->maxchild = MAXCHILD;
    ap->maxforkdepth = MAXFORKDEPTH;

    while ( (ch = getopt(argc, argv, "am:M:hs:S:v")) != -1) {
        switch (ch) {
            case 'm': {
                u_int16_t n = (u_int16_t)atoi(optarg);
                ap->maxchild = n > MAXCHILD ? MAXCHILD : n;
                }
                break;
            case 'M':
                ap->maxforkdepth = (u_int16_t)atoi(optarg);
                break;
            case 's':
                ALCOVE_SETOPT(ap, alcove_opt_exit_status, atoi(optarg));
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
     */
    if (open("/dev/null", O_RDWR) != 3)
        erl_err_quit("could not acquire ctl fd");

    alcove_event_loop(ap);
    exit(0);
}

    void
alcove_event_loop(alcove_state_t *ap)
{
    fd_set rfds;
    int fdmax = 0;

    erl_init(NULL, 0);

    if (ap->fdsetsize != ap->maxchild) {
        ap->fdsetsize = ap->maxchild;
        ap->child = realloc(ap->child,
                sizeof(alcove_child_t) * ap->fdsetsize);

        if (!ap->child)
            erl_err_sys("realloc");
    }

    (void)memset(ap->child, 0, sizeof(alcove_child_t) * ap->fdsetsize);

    sigcaught = 0;

    for ( ; ; ) {
        if (alcove_handle_signal(ap) < 0)
            erl_err_sys("alcove_handle_signal");

        FD_ZERO(&rfds);
        FD_SET(STDIN_FILENO, &rfds);

        fdmax = STDIN_FILENO;

        (void)pid_foreach(ap, 0, &rfds, &fdmax, pid_not_equal, set_pid);

        if (select(fdmax+1, &rfds, NULL, NULL, NULL) < 0) {
            switch (errno) {
                case EAGAIN:
                case EINTR:
                    continue;
                default:
                    break;
            }
        }

        if (FD_ISSET(STDIN_FILENO, &rfds)) {
            switch (alcove_stdin(ap)) {
                case -1:
                    erl_err_sys("alcove_stdin");
                case 1:
                    /* EOF */
                    return;
                case 0:
                    break;
            }
        }

        pid_foreach(ap, 0, &rfds, &ap->depth, pid_not_equal, read_from_pid);

        if (ap->verbose > 1)
            alcove_stats(ap);
    }
}

    static int
alcove_stdin(alcove_state_t *ap)
{
    u_int16_t bufsz = 0;
    u_int16_t type = 0;
    pid_t pid = 0;
    unsigned char buf[65535] = {0};

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
    if (alcove_read(STDIN_FILENO, &bufsz, sizeof(bufsz)) != sizeof(bufsz)) {
        if (errno == 0)
            return 1;

        return -1;
    }

    bufsz = ntohs(bufsz);
    bufsz -= sizeof(type);

    /* message type */
    if (alcove_read(STDIN_FILENO, &type, sizeof(type)) != sizeof(type))
        return -1;

    if (type == ALCOVE_MSG_CALL) {
        if (alcove_msg_call(ap, STDIN_FILENO, bufsz) < 0)
            return -1;
    }
    else if (type == ALCOVE_MSG_STDIN) {
        if (alcove_read(STDIN_FILENO, &pid, sizeof(pid)) != sizeof(pid))
            return -1;

        pid = ntohl(pid);
        bufsz -= sizeof(pid);

        if (alcove_read(STDIN_FILENO, buf, bufsz) != bufsz)
            return -1;

        switch (pid_foreach(ap, pid, buf, &bufsz, pid_equal, write_to_pid)) {
            case -2:
                return -1;
            case -1:
                /* XXX badarg */
            case 0:
                break;
        }
    }
    else {
        /* XXX return badarg to caller */
        return -1;
    }

    return 0;
}

    static ssize_t
alcove_msg_call(alcove_state_t *ap, int fd, u_int16_t len)
{
    alcove_msg_t *msg = NULL;
    ETERM *arg = NULL;
    ETERM *reply = NULL;
    ssize_t rv = -1;

    msg = alcove_msg_read(fd, len);
    if (!msg)
        return -1;

    arg = erl_decode(msg->arg);
    if (!arg)
        goto DONE;

    reply = alcove_call(ap, msg->call, arg);
    if (!reply)
        goto DONE;

    rv = alcove_send(ALCOVE_MSG_CALL, reply);

DONE:
    free(msg->arg);
    free(msg);
    erl_free_compound(arg);
    erl_free_compound(reply);

    return rv;
}

    static alcove_msg_t *
alcove_msg_read(int fd, u_int16_t len)
{
    u_int16_t call = 0;
    alcove_msg_t *msg = NULL;

    if (len <= sizeof(call))
        return NULL;

    len -= sizeof(call);

    /* call */
    if (alcove_read(fd, &call, sizeof(call)) != sizeof(call))
        return NULL;

    msg = alcove_malloc(sizeof(alcove_msg_t));
    msg->call = ntohs(call);

    /* arg */
    msg->arg = alcove_malloc(len);

    if (alcove_read(fd, msg->arg, len) != len)
        goto BADARG;

    return msg;

BADARG:
    if (msg) {
        if (msg->arg)
            free(msg->arg);

        free(msg);
    }

    return NULL;
}

    static ssize_t
alcove_child_stdio(int fdin, u_int16_t depth, alcove_child_t *c, u_int16_t type)
{
    ssize_t n = 0;
    alcove_msg_stdio_t *msg = NULL;
    unsigned char buf[MAXMSGLEN] = {0};
    size_t read_len = 2;
    ssize_t rv = 0;

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
        n = buf[0] << 8 | buf[1];

        if (n > sizeof(buf) - 2)
            return -1;

        if (alcove_read(fdin, buf+2, n) != n)
            return -1;

        n += 2;
    }

    msg = alcove_alloc_hdr_stdio(c->pid, type, buf, (size_t *)&n);
    if (!msg)
        return -1;

    rv = alcove_write(msg, n);

    free(msg);

    return rv;
}

    static ssize_t
alcove_send(u_int16_t type, ETERM *t)
{
    alcove_msg_call_t *msg = NULL;
    size_t len = 0;
    ssize_t n = 0;

    msg = alcove_alloc_hdr_call(type, t, &len);
    if (!msg)
        return -1;

    n = alcove_write(msg, len);

    free(msg);

    return n;
}

    static ssize_t
alcove_call_stdio(pid_t pid, u_int16_t type, ETERM *t)
{
    alcove_msg_call_t *call = NULL;
    alcove_msg_stdio_t *msg = NULL;
    size_t len = 0;
    ssize_t n = -1;

    call = alcove_alloc_hdr_call(type, t, &len);
    if (!call)
        return -1;

    msg = alcove_alloc_hdr_stdio(pid, ALCOVE_MSG_PROXY, call, &len);
    if (!msg)
        goto ERR;

    n = alcove_write(msg, len);

ERR:
    free(call);
    free(msg);

    return n;
}

    static alcove_msg_stdio_t *
alcove_alloc_hdr_stdio(pid_t pid, u_int16_t type, void *buf, size_t *len)
{
    alcove_msg_stdio_t *msg = NULL;
    size_t msg_len = 0;

    if (*len > sizeof(msg->buf))
        return NULL;

    msg = alcove_malloc(sizeof(alcove_msg_stdio_t));
    (void)memcpy(msg->buf, buf, *len);

    msg_len = sizeof(msg->type) + sizeof(msg->pid) + *len;
    msg->len = htons(msg_len);
    msg->type = type;
    msg->pid = htonl(pid);

    *len = sizeof(msg->len) + msg_len;

    return msg;
}

    static alcove_msg_call_t *
alcove_alloc_hdr_call(u_int16_t type, ETERM *t, size_t *len)
{
    alcove_msg_call_t *msg = NULL;
    int term_len = 0;

    msg = alcove_malloc(sizeof(alcove_msg_call_t));

    term_len = erl_term_len(t);
    if (term_len < 0 || term_len > sizeof(msg->buf))
        goto ERR;

    if (erl_encode(t, msg->buf) < 1)
        goto ERR;

    msg->len = htons(sizeof(msg->type) + term_len);
    msg->type = type;

    *len = sizeof(msg->len) + sizeof(msg->type) + term_len;

    return msg;

ERR:
    free(msg);

    return NULL;
}

    static ssize_t
alcove_write(void *data, size_t len)
{
    ssize_t n = 0;

    flockfile(stdout);
    n = write(STDOUT_FILENO, data, len);
    funlockfile(stdout);

    return (n == len ? n : -1);
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
        int (*comp)(pid_t, pid_t), int (*fp)(alcove_child_t *, void *, void *))
{
    int i = 0;
    int rv = 0;

    for (i = 0; i < ap->fdsetsize; i++) {
        if ((*comp)(ap->child[i].pid, pid) == 0)
            continue;

        rv = (*fp)(&(ap->child[i]), arg1, arg2);

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
exited_pid(alcove_child_t *c, void *arg1, void *arg2)
{
    int *status = arg1;
    int *opt = arg2;
    ETERM *t = NULL;

    c->exited = 1;
    (void)close(c->fdin);
    c->fdin = -1;

    if (WIFEXITED(*status) && (*opt & alcove_opt_exit_status)) {
        t = alcove_tuple2(
                erl_mk_atom("exit_status"),
                erl_mk_int(WEXITSTATUS(*status))
                );

        if (alcove_call_stdio(c->pid, ALCOVE_MSG_EVENT, t))
            return -1;
    }

    if (WIFSIGNALED(*status) && (*opt & alcove_opt_termsig)) {
        t = alcove_tuple2(
                erl_mk_atom("termsig"),
                erl_mk_int(WTERMSIG(*status))
                );

        if (alcove_call_stdio(c->pid, ALCOVE_MSG_EVENT, t) < 0)
            return -1;
    }

    return 0;
}

    static int
set_pid(alcove_child_t *c, void *arg1, void *arg2)
{
    fd_set *rfds = arg1;
    int *fdmax = arg2;

    if (c->fdctl > -1) {
        FD_SET(c->fdctl, rfds);
        *fdmax = MAX(*fdmax, c->fdctl);
    }

    if (c->fdout > -1) {
        FD_SET(c->fdout, rfds);
        *fdmax = MAX(*fdmax, c->fdout);
    }

    if (c->fderr > -1) {
        FD_SET(c->fderr, rfds);
        *fdmax = MAX(*fdmax, c->fderr);
    }

    if (c->exited && c->fdout == -1 && c->fderr == -1) {
        c->pid = 0;
        c->exited = 0;
    }

    return 1;
}

    static int
write_to_pid(alcove_child_t *c, void *arg1, void *arg2)
{
    char *buf = arg1;
    u_int16_t *bufsz = arg2;

    if (c->fdin == -1)
        return -1;

    if (write(c->fdin, buf, *bufsz) != *bufsz)
        return -2;

    return 0;
}

    static int
read_from_pid(alcove_child_t *c, void *arg1, void *arg2)
{
    fd_set *rfds = arg1;
    u_int16_t *depth = arg2;

    if (c->fdctl > -1 && FD_ISSET(c->fdctl, rfds)) {
        unsigned char buf;
        ssize_t n;

        n = read(c->fdctl, &buf, sizeof(buf));
        (void)close(c->fdctl);
        c->fdctl = -1;

        if (n == 0) {
            c->fdctl = ALCOVE_CHILD_EXEC;
            if (alcove_call_stdio(c->pid, ALCOVE_MSG_CALL,
                        erl_mk_atom("ok")) < 0)
                return -1;
        }
    }

    if (c->fdout > -1 && FD_ISSET(c->fdout, rfds)) {
        switch (alcove_child_stdio(c->fdout, *depth, c, ALCOVE_MSG_TYPE(c))) {
            case -1:
            case 0:
                (void)close(c->fdout);
                c->fdout = -1;
                break;
            default:
                break;
        }
    }

    if (c->fderr > -1 && FD_ISSET(c->fderr, rfds)) {
        switch (alcove_child_stdio(c->fderr, *depth, c, ALCOVE_MSG_STDERR)) {
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

        if (signum == SIGCHLD) {
            pid_t pid = 0;

            for ( ; ; ) {
                errno = 0;
                pid = waitpid(-1, &status, WNOHANG);

                if (errno == ECHILD || pid == 0)
                    break;

                if (pid < 0)
                    return -1;

                (void)pid_foreach(ap, pid, &status, &ap->opt,
                        pid_equal, exited_pid);
            }
        }

        reply = alcove_tuple2(
                erl_mk_atom("signal"),
                erl_mk_int(signum)
                );

        if (alcove_send(ALCOVE_MSG_EVENT, reply) < 0) {
            erl_free_compound(reply);
            goto DONE;
        }

        erl_free_compound(reply);

        sigcaught &= ~(1 << signum);
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
            "   -s <0|1>        child exit status\n"
            "   -S <0|1>        child termination signal\n"
            "   -v              verbose mode\n",
            __progname
            );

    exit (EXIT_FAILURE);
}
