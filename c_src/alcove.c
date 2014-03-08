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
#include <wait.h>

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
    u_int16_t len;
    u_int16_t type;
    pid_t pid;
    unsigned char buf[65535];
} alcove_msg_stdio_t;

typedef struct {
    u_int16_t len;
    u_int16_t type;
    unsigned char buf[65535];
} alcove_msg_call_t;

static ssize_t alcove_call(alcove_state_t *ap, int fd, u_int16_t len);
static alcove_msg_t * alcove_msg(int fd, u_int16_t len);

static ssize_t alcove_child_stdio(int fdin, pid_t pid, u_int16_t type);
static ssize_t alcove_write(u_int16_t, ETERM *);
static ssize_t alcove_read(int, void *, ssize_t);

static void alcove_stats(alcove_state_t *ap);
static void usage(alcove_state_t *);

extern char *__progname;

static int child_exited = 0;

    void
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

    ap = calloc(1, sizeof(alcove_state_t));
    if (!ap)
        erl_err_sys("calloc");

    ap->child = calloc(ALCOVE_MAX_CHILD, sizeof(alcove_child_t));
    if (!ap->child)
        erl_err_sys("calloc");

    signal(SIGCHLD, gotsig);

    while ( (ch = getopt(argc, argv, "hv")) != -1) {
        switch (ch) {
            case 'v':
                ap->verbose++;
                break;
            case 'h':
            default:
                usage(ap);
        }
    }

    alcove_ctl(ap);
    exit(0);
}

    void
alcove_ctl(alcove_state_t *ap)
{
    fd_set rfds;
    int fdmax = 0;
    int rv = 0;

    erl_init(NULL, 0);

    (void)memset(ap->child, 0, sizeof(alcove_child_t) * ALCOVE_MAX_CHILD);
    ap->nchild = 0;

    for ( ; ; ) {
        int i = 0;

        if (child_exited) {
            pid_t pid = 0;

            pid = waitpid(-1, 0, WNOHANG);

            switch (pid) {
                case -1:
                    erl_err_sys("waitpid");
                case 0:
                    break;
                default:
                    for (i = 0; i < ALCOVE_MAX_CHILD; i++) {
                        if (ap->child[i].pid != pid)
                            continue;

                        ap->child[i].pid = 0;
                        ap->child[i].fdin = -1;
                        ap->child[i].fdout = -1;
                        ap->child[i].fderr = -1;

                        break;
                    }
            }
        }

        FD_ZERO(&rfds);
        FD_SET(STDIN_FILENO, &rfds);
        fdmax = STDIN_FILENO;

        for (i = 0; i < ALCOVE_MAX_CHILD; i++) {
            if (ap->child[i].pid == 0)
                continue;

            if (ap->child[i].fdout > -1) {
                FD_SET(ap->child[i].fdout, &rfds);
                fdmax = MAX(fdmax, ap->child[i].fdout);
            }

            if (ap->child[i].fderr > -1) {
                FD_SET(ap->child[i].fderr, &rfds);
                fdmax = MAX(fdmax, ap->child[i].fderr);
            }

            break;
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

        if (FD_ISSET(STDIN_FILENO, &rfds)) {
            u_int16_t bufsz = 0;
            u_int16_t type = 0;
            pid_t pid = 0;
            char buf[65535] = {0};
            int i = 0;

            errno = 0;

            /* total length, not including length header */
            if (alcove_read(STDIN_FILENO, &bufsz, sizeof(bufsz)) != sizeof(bufsz)) {
                if (errno == 0)
                    return;

                erl_err_sys("read:stdin");
            }

            bufsz = ntohs(bufsz);
            bufsz -= sizeof(type);

            /* message type */
            if (alcove_read(STDIN_FILENO, &type, sizeof(type)) != sizeof(type))
                erl_err_sys("read:stdin");

            if (type == ALCOVE_MSG_CALL) {
                if (alcove_call(ap, STDIN_FILENO, bufsz) < 0)
                    erl_err_quit("call");
            }
            else if (type == ALCOVE_MSG_CHILDIN) {
                if (alcove_read(STDIN_FILENO, &pid, sizeof(pid)) != sizeof(pid))
                    erl_err_sys("read:stdin");

                pid = ntohl(pid);
                bufsz -= sizeof(pid);

                if (alcove_read(STDIN_FILENO, buf, bufsz) != bufsz)
                    erl_err_sys("read:stdin");

                for (i = 0; i < ALCOVE_MAX_CHILD; i++) {
                    if (pid != ap->child[i].pid)
                        continue;

                    /* XXX badarg */
                    if (ap->child[i].fdin == -1)
                        erl_err_quit("%d:fd closed", pid);

                    if (write(ap->child[i].fdin, buf, bufsz) != bufsz)
                        erl_err_sys("write:stdin->childin");

                    break;
                }
            }
            else {
                /* XXX return badarg to caller */
                erl_err_quit("bad message type:%u", type);
            }
        }

        for (i = 0; i < ALCOVE_MAX_CHILD; i++) {
            if (ap->child[i].pid == 0)
                continue;

            if (FD_ISSET(ap->child[i].fdout, &rfds)) {
                switch (alcove_child_stdio(ap->child[i].fdout, ap->child[i].pid, ALCOVE_MSG_CHILDOUT)) {
                    case -1:
                    case 0:
                        ap->child[i].fdout = -1;
                        break;
                    default:
                        break;
                }
            }

            if (FD_ISSET(ap->child[i].fderr, &rfds)) {
                switch (alcove_child_stdio(ap->child[i].fderr, ap->child[i].pid, ALCOVE_MSG_CHILDERR)) {
                    case -1:
                    case 0:
                        ap->child[i].fderr = -1;
                        break;
                    default:
                        break;
                }
            }
        }
    }
}

    static ssize_t
alcove_call(alcove_state_t *ap, int fd, u_int16_t len)
{
    alcove_msg_t *msg = NULL;
    ETERM *arg = NULL;
    ETERM *reply = NULL;
    ssize_t rv = -1;

    msg = alcove_msg(fd, len);
    if (!msg)
        return -1;

    arg = erl_decode(msg->arg);
    if (!arg)
        goto DONE;

    reply = alcove_cmd(ap, msg->cmd, arg);
    if (!reply)
        goto DONE;

    rv = alcove_write(ALCOVE_MSG_CALL, reply);

DONE:
    free(msg->arg);
    free(msg);
    erl_free_compound(arg);
    erl_free_compound(reply);

    return rv;
}

    static alcove_msg_t *
alcove_msg(int fd, u_int16_t len)
{
    u_int16_t cmd = 0;
    alcove_msg_t *msg = NULL;

    if (len <= sizeof(cmd))
        return NULL;

    len -= sizeof(cmd);

    /* cmd */
    if (alcove_read(fd, &cmd, sizeof(cmd)) != sizeof(cmd))
        return NULL;

    msg = alcove_malloc(sizeof(alcove_msg_t));
    msg->cmd = ntohs(cmd);

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
alcove_child_stdio(int fdin, pid_t pid, u_int16_t type)
{
    ssize_t n = 0;
    u_int16_t packet_len = 0;
    alcove_msg_stdio_t msg = {0};

    errno = 0;
    n = read(fdin, msg.buf, sizeof(msg.buf));

    if (n <= 0) {
        if (errno == 0)
            return 0;

        return -1;
    }

    msg.len = htons(sizeof(type)+sizeof(pid)+n);

    msg.type = type;
    msg.pid = htonl(pid);

    packet_len = sizeof(msg.len) + sizeof(msg.type) + sizeof(msg.pid) + n;

    flockfile(stdout);
    n = write(STDOUT_FILENO, &msg, packet_len);
    funlockfile(stdout);

    return (n == packet_len ? n : -1);
}

    static ssize_t
alcove_write(u_int16_t type, ETERM *t)
{
    ssize_t n = 0;
    int term_len = 0;
    u_int16_t packet_len = 0;
    alcove_msg_call_t msg = {0};

    term_len = erl_term_len(t);
    if (term_len < 0 || term_len+sizeof(msg.len)+sizeof(msg.type) >= UINT16_MAX)
        goto ERR;

    msg.len = htons(term_len+sizeof(msg.type));

    if (erl_encode(t, msg.buf) < 1)
        goto ERR;

    packet_len = sizeof(msg.len) + sizeof(msg.type) + term_len;

    flockfile(stdout);
    n = write(STDOUT_FILENO, &msg, packet_len);
    funlockfile(stdout);

    return (n == packet_len ? n : -1);

ERR:
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
            "   -v              verbose mode\n",
            __progname
            );

    exit (EXIT_FAILURE);
}
