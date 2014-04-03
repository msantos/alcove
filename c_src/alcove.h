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

#include <erl_driver.h>
#include <erl_interface.h>

#include "erl_errno.h"
#include "alcove_version.h"

/* 4 pipes per child */
#define MAXCHILD        (FD_SETSIZE / 4 - 4)
#define MAXFORKDEPTH    16
#define MAXMSGLEN       65535
#define MAXHDRLEN       8 /* 2 bytes length + 2 bytes type + 4 bytes PID */

#define ALCOVE_MSGLEN(x,n) \
    ((n) - (((x) + 1) * MAXHDRLEN))

#define ALCOVE_DEFINE(x) {#x, x}

#define ALCOVE_IS_UNSIGNED_INTEGER(x) \
    (ERL_IS_INTEGER(x) || ERL_IS_UNSIGNED_INTEGER(x))

#define ALCOVE_IS_UNSIGNED_LONG(x) \
    (ERL_IS_INTEGER(x) || ERL_IS_UNSIGNED_INTEGER(x) \
     || ERL_IS_LONGLONG(x) || ERL_IS_UNSIGNED_LONGLONG(x))

#define ALCOVE_IS_UNSIGNED_LONGLONG(x) \
    (ERL_IS_INTEGER(x) || ERL_IS_UNSIGNED_INTEGER(x) \
     || ERL_IS_LONGLONG(x) || ERL_IS_UNSIGNED_LONGLONG(x))

#define ALCOVE_IS_LONG(x) \
    (ERL_IS_INTEGER(x) || ERL_IS_LONGLONG(x))

#define ALCOVE_IS_LONGLONG(x) \
    (ERL_IS_INTEGER(x) || ERL_IS_LONGLONG(x))

#define ALCOVE_LL_UVALUE(x) \
    ((ERL_IS_INTEGER(x) || ERL_IS_UNSIGNED_INTEGER(x)) \
        ? ERL_INT_UVALUE(x) \
        : ERL_LL_UVALUE(x))

enum {
    alcove_opt_exit_status = 1 << 0,   /* Report child exit status */
    alcove_opt_termsig = 1 << 1,       /* Report child termination signal */
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
    u_int16_t maxchild;
    u_int16_t maxforkdepth;
    u_int16_t fdsetsize;
    u_int16_t depth;
    alcove_child_t *child;
} alcove_state_t;

typedef struct {
    u_int32_t call;
    unsigned char *arg;
} alcove_msg_t;

typedef struct {
    char *name;
    long long val;
} alcove_define_t;

void sighandler(int sig);

void alcove_event_loop(alcove_state_t *ap);

int pid_foreach(alcove_state_t *ap, pid_t pid, void *arg1, void *arg2,
        int (*comp)(pid_t, pid_t), int (*fp)(alcove_child_t *, void *, void *));
int pid_equal(pid_t p1, pid_t p2);
int pid_not_equal(pid_t p1, pid_t p2);

void *alcove_malloc(ssize_t size);

ETERM *alcove_list_head(ETERM **, ETERM *);
ETERM *alcove_tuple2(ETERM *, ETERM *);
ETERM *alcove_tuple3(ETERM *, ETERM *, ETERM *);
ETERM *alcove_errno(int);
ETERM *alcove_error(const char *);
ETERM *alcove_ok(ETERM *);
ETERM *alcove_bool(bool);
ETERM *alcove_bin(const char *);
ETERM *alcove_define(char *name, alcove_define_t *constants);
ETERM *alcove_constant(u_int64_t val, alcove_define_t *constants);

ETERM *alcove_call(alcove_state_t *, u_int32_t, ETERM *);

ETERM *signum_to_atom(int signum);

#define VERBOSE(x, ...) do { \
    if (ap->verbose >= x) { \
        erl_err_msg(__VA_ARGS__); \
    } \
} while (0)
