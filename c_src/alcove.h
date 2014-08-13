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
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/uio.h>
#include <stdbool.h>
#include <sys/param.h>
#include <sys/resource.h>
#include <sys/types.h>
#include <signal.h>
#include <sys/socket.h>

#include <arpa/inet.h>
#include <stdint.h>

#include <fcntl.h>
#include <sys/types.h>

#include <ei.h>

#include <err.h>

#include <err.h>

#include "erl_errno.h"
#include "alcove_version.h"

#define ALCOVE_FDCTL 3

#define MAXFORKDEPTH    16
#define MAXMSGLEN       UINT16_MAX
#define MAXHDRLEN       8 /* 2 bytes length + 2 bytes type + 4 bytes PID */

#define ALCOVE_MSGLEN(x,n) \
    ((n) - (((x) + 1) * MAXHDRLEN))

#define ALCOVE_DEFINE(x) {#x, x}

#define ALCOVE_SETOPT(x,k,v) \
    (x)->opt = (v) ? (x)->opt | (k) : (x)->opt & ~(k)

#define ALCOVE_ERR(_x) \
    if ((_x) < 0) errx(EXIT_FAILURE, "internal error")

#define ALCOVE_TUPLE2(_msg, _index, _tag, _term) do { \
    ALCOVE_ERR(ei_encode_version(_msg, _index)); \
    ALCOVE_ERR(ei_encode_tuple_header(_msg, _index, 2)); \
    ALCOVE_ERR(ei_encode_atom(_msg, _index, _tag)); \
    ALCOVE_ERR(_term); \
    } while (0)

#define ALCOVE_TUPLE3(_msg, _index, _tag, _term1, _term2) do { \
    ALCOVE_ERR(ei_encode_version(_msg, _index)); \
    ALCOVE_ERR(ei_encode_tuple_header(_msg, _index, 3)); \
    ALCOVE_ERR(ei_encode_atom(_msg, _index, _tag)); \
    ALCOVE_ERR(_term1); \
    ALCOVE_ERR(_term2); \
    } while (0)

#define ALCOVE_TUPLE4(_msg, _index, _tag, _term1, _term2, _term3) do { \
    ALCOVE_ERR(ei_encode_version(_msg, _index)); \
    ALCOVE_ERR(ei_encode_tuple_header(_msg, _index, 4)); \
    ALCOVE_ERR(ei_encode_atom(_msg, _index, _tag)); \
    ALCOVE_ERR(_term1); \
    ALCOVE_ERR(_term2); \
    ALCOVE_ERR(_term3); \
    } while (0)

#define ALCOVE_TUPLE5(_msg, _index, _tag, _term1, _term2, _term3, _term4) do { \
    ALCOVE_ERR(ei_encode_version(_msg, _index)); \
    ALCOVE_ERR(ei_encode_tuple_header(_msg, _index, 3)); \
    ALCOVE_ERR(ei_encode_atom(_msg, _index, _tag)); \
    ALCOVE_ERR(_term1); \
    ALCOVE_ERR(_term2); \
    ALCOVE_ERR(_term3); \
    ALCOVE_ERR(_term4); \
    } while (0)

#define ALCOVE_OK(_msg, _index, _buf) \
    ALCOVE_TUPLE2(_msg, _index, "ok", _buf)

#define get_int32(s) ((((unsigned char*) (s))[0] << 24) | \
                      (((unsigned char*) (s))[1] << 16) | \
                      (((unsigned char*) (s))[2] << 8)  | \
                      (((unsigned char*) (s))[3]))

#define put_int32(i, s) do {((char*)(s))[0] = (char)((i) >> 24) & 0xff;   \
                            ((char*)(s))[1] = (char)((i) >> 16) & 0xff;   \
                            ((char*)(s))[2] = (char)((i) >> 8)  & 0xff;   \
                            ((char*)(s))[3] = (char)(i)         & 0xff;} \
                        while (0)

#define get_int16(s) ((((unsigned char*)  (s))[0] << 8) | \
                      (((unsigned char*)  (s))[1]))

#define put_int16(i, s) do {((char*)(s))[0] = (char)((i) >> 8) & 0xff;  \
                            ((char*)(s))[1] = (char)(i)        & 0xff;} \
                        while (0)

enum {
    alcove_opt_exit_status = 1 << 0,   /* Report child exit status */
    alcove_opt_termsig = 1 << 1,       /* Report child termination signal */
    alcove_opt_sigchld = 1 << 2,       /* Report SIGCHLD */
};

typedef struct {
    pid_t pid;
    int exited;
    int fdctl;
    int fdin;
    int fdout;
    int fderr;
} alcove_child_t;

typedef struct {
    u_int32_t opt;
    u_int8_t verbose;
    long maxfd;
    u_int16_t maxchild;
    u_int16_t maxforkdepth;
    u_int16_t fdsetsize;
    u_int16_t depth;
    alcove_child_t *child;
} alcove_state_t;

typedef struct {
    char *name;
    long long val;
} alcove_define_t;

typedef struct {
    void *p;
    size_t len;
} alcove_alloc_t;

void sighandler(int sig);

void alcove_event_loop(alcove_state_t *ap);

int pid_foreach(alcove_state_t *ap, pid_t pid, void *arg1, void *arg2,
        int (*comp)(pid_t, pid_t),
        int (*fp)(alcove_state_t *, alcove_child_t *, void *, void *));
int pid_equal(pid_t p1, pid_t p2);
int pid_not_equal(pid_t p1, pid_t p2);

int alcove_decode_int(const char *, int *, int *);
int alcove_decode_uint(const char *, int *, u_int32_t *);

ssize_t alcove_decode_iolist_to_binary(const char *buf, int *index,
        char *res, size_t *rlen);
ssize_t alcove_errno(char *buf, size_t len, int errnum);
ssize_t alcove_error(char *buf, size_t len, const char *reason);
ssize_t alcove_mk_atom(char *buf, size_t len, const char *atom);
ssize_t alcove_mk_binary(char *buf, size_t buflen, void *bin, size_t len);
ssize_t alcove_mk_long(char *buf, size_t buflen, long n);
ssize_t alcove_mk_ulong(char *buf, size_t buflen, unsigned long n);
void *alcove_malloc(ssize_t size);
int alcove_define(char *buf, int *index, char *name,
        alcove_define_t *constants);
int alcove_constant(char *buf, int *index, u_int64_t val,
        alcove_define_t *constants);
char **alcove_list_to_argv(const char *, int *);
void alcove_free_argv(char **);

ssize_t alcove_call(alcove_state_t *ap, u_int32_t call,
        const char *arg, size_t len, char *reply, size_t rlen);

char *erl_errno_id(int error);

#define VERBOSE(x, ...) do { \
    if (ap->verbose >= x) { \
        warnx(__VA_ARGS__); \
    } \
} while (0)
