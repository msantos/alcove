/* Copyright (c) 2014-2022, Michael Santos <michael.santos@gmail.com>
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
#if defined(__linux__)
#define _GNU_SOURCE
#endif

#include <limits.h>
#include <signal.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <sys/param.h>
#include <sys/resource.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>

#include <arpa/inet.h>
#include <stdint.h>

#include <fcntl.h>

#include <ei.h>

#include "erl_errno.h"

#include "alcove_nr.h"
#include "alcove_version.h"

#include "recallocarray.h"
#include "setproctitle.h"

#if defined(__sun) && defined(__SVR4)
#define u_int8_t uint8_t
#define u_int16_t uint16_t
#define u_int32_t uint32_t
#define u_int64_t uint64_t

#define __sunos__
#endif

#ifndef MAX
#define MAX(x, y) ((x) > (y) ? (x) : (y))
#endif

#ifndef MIN
#define MIN(x, y) ((x) < (y) ? (x) : (y))
#endif

#define UNUSED(x) (void)x

#define MAXPROC 64
#define MAXFORKDEPTH 16
#define MAXMSGLEN UINT16_MAX
#define MAXHDRLEN 8 /* 2 bytes length + 2 bytes type + 4 bytes PID */

#define ALCOVE_MSGLEN(x, n) ((n) - (((x) + 1) * MAXHDRLEN))

#define ALCOVE_CONSTANT(x)                                                     \
  { #x, x }

#define ALCOVE_SETOPT(x, k, v) (x)->opt = (v) ? (x)->opt | (k) : (x)->opt & ~(k)

#define ALCOVE_ERR(_x)                                                         \
  if ((_x) < 0)                                                                \
    exit(111);

#define ALCOVE_TUPLE2(_msg, _len, _index, _tag, _term)                         \
  do {                                                                         \
    *(_index) = 0;                                                             \
    ALCOVE_ERR(alcove_encode_version(_msg, _len, _index));                     \
    ALCOVE_ERR(alcove_encode_tuple_header(_msg, _len, _index, 2));             \
    ALCOVE_ERR(alcove_encode_atom(_msg, _len, _index, _tag));                  \
    ALCOVE_ERR(_term);                                                         \
  } while (0)

#define ALCOVE_TUPLE3(_msg, _len, _index, _tag, _term1, _term2)                \
  do {                                                                         \
    *(_index) = 0;                                                             \
    ALCOVE_ERR(alcove_encode_version(_msg, _len, _index));                     \
    ALCOVE_ERR(alcove_encode_tuple_header(_msg, _len, _index, 3));             \
    ALCOVE_ERR(alcove_encode_atom(_msg, _len, _index, _tag));                  \
    ALCOVE_ERR(_term1);                                                        \
    ALCOVE_ERR(_term2);                                                        \
  } while (0)

#define ALCOVE_TUPLE4(_msg, _len, _index, _tag, _term1, _term2, _term3)        \
  do {                                                                         \
    *(_index) = 0;                                                             \
    ALCOVE_ERR(alcove_encode_version(_msg, _len, _index));                     \
    ALCOVE_ERR(alcove_encode_tuple_header(_msg, _len, _index, 4));             \
    ALCOVE_ERR(alcove_encode_atom(_msg, _len, _index, _tag));                  \
    ALCOVE_ERR(_term1);                                                        \
    ALCOVE_ERR(_term2);                                                        \
    ALCOVE_ERR(_term3);                                                        \
  } while (0)

#define ALCOVE_OK(_msg, _len, _index, _buf)                                    \
  ALCOVE_TUPLE2(_msg, _len, _index, "ok", _buf)

#define get_int32(s)                                                           \
  ((((const unsigned char *)(s))[0] << 24) |                                   \
   (((const unsigned char *)(s))[1] << 16) |                                   \
   (((const unsigned char *)(s))[2] << 8) | (((const unsigned char *)(s))[3]))

#define put_int32(i, s)                                                        \
  do {                                                                         \
    ((char *)(s))[0] = (char)((i) >> 24) & 0xff;                               \
    ((char *)(s))[1] = (char)((i) >> 16) & 0xff;                               \
    ((char *)(s))[2] = (char)((i) >> 8) & 0xff;                                \
    ((char *)(s))[3] = (char)(i)&0xff;                                         \
  } while (0)

#define get_int16(s)                                                           \
  ((((const unsigned char *)(s))[0] << 8) | (((const unsigned char *)(s))[1]))

#define put_int16(i, s)                                                        \
  do {                                                                         \
    ((char *)(s))[0] = (char)((i) >> 8) & 0xff;                                \
    ((char *)(s))[1] = (char)(i)&0xff;                                         \
  } while (0)

#define get_int8(s) (((const unsigned char *)(s))[0])

enum {
  alcove_opt_stdin_closed = 1 << 0,  /* Report child stdin closed */
  alcove_opt_stdout_closed = 1 << 1, /* Report child stdout closed */
  alcove_opt_stderr_closed = 1 << 2, /* Report child stderr closed */
  alcove_opt_exit_status = 1 << 3,   /* Report child exit status */
  alcove_opt_termsig = 1 << 4,       /* Report child termination signal */
  alcove_opt_signaloneof = 1 << 5    /* Signal child processes on EOF */
};

enum {
  ALCOVE_SIGREAD_FILENO = 3,
  ALCOVE_SIGWRITE_FILENO,
  ALCOVE_FDCTL_FILENO,
  ALCOVE_MAXFILENO
};

/* Calculate the number of file descriptors required to handle the number
 * of subprocesses:
 *
 * * the control process requires ALCOVE_MAXFILENO file descriptors
 * * each subprocess requires ALCOVE_MAXFILENO file descriptors
 *
 */
#define ALCOVE_NFD(_nproc) ((_nproc)*ALCOVE_MAXFILENO + ALCOVE_MAXFILENO)

/* Calculate the number of subprocesses supported by a file descriptor limit */
#define ALCOVE_MAXCHILD(_nfds) (((_nfds)-ALCOVE_MAXFILENO) / ALCOVE_MAXFILENO)

typedef struct {
  pid_t pid;
  int flowcontrol;
  int signaloneof;
  int exited;
  int fdctl;
  int fdin;
  int fdout;
  int fderr;
} alcove_child_t;

typedef struct {
  int32_t opt;
  rlim_t maxfd;
  u_int8_t sigchld;
  u_int16_t maxchild;
  u_int16_t maxforkdepth;
  u_int16_t fdsetsize;
  u_int16_t depth;
  int flowcontrol;
  int signaloneof;
  alcove_child_t *child;
  uint8_t filter[ALCOVE_NR_SIZE];
  uint8_t filter1[ALCOVE_NR_SIZE];
} alcove_state_t;

typedef struct {
  char *name;
  long long val;
} alcove_constant_t;

typedef struct {
  void *p;
  size_t len;
} alcove_alloc_t;

void alcove_sig_info(int sig, siginfo_t *info, void *context);

void alcove_event_init(alcove_state_t *ap);
void alcove_event_loop(alcove_state_t *ap);

int pid_foreach(alcove_state_t *ap, pid_t pid, void *arg1, void *arg2,
                int (*comp)(pid_t, pid_t),
                int (*fp)(alcove_state_t *, alcove_child_t *, void *, void *));
int pid_equal(pid_t p1, pid_t p2);
int pid_not_equal(pid_t p1, pid_t p2);
int fdlimit_pid(alcove_state_t *ap, alcove_child_t *c, void *arg1, void *arg2);

ssize_t alcove_signal_name(char *, size_t, int *, int);

int alcove_get_type(const char *, size_t, const int *, int *, int *);
int alcove_decode_binary(const char *, size_t, int *, void *, size_t *);
int alcove_decode_int(const char *, size_t, int *, int *);
int alcove_decode_uint(const char *, size_t, int *, u_int32_t *);
int alcove_decode_long(const char *, size_t, int *, long *);
int alcove_decode_ulong(const char *, size_t, int *, unsigned long *);
int alcove_decode_longlong(const char *, size_t, int *, long long *);
int alcove_decode_ulonglong(const char *, size_t, int *, unsigned long long *);
int alcove_decode_atom(const char *, size_t, int *, char *);
int alcove_decode_string(const char *, size_t, int *, char *, size_t);
int alcove_decode_list_header(const char *, size_t, int *, int *);
int alcove_decode_tuple_header(const char *, size_t, int *, int *);
int alcove_decode_iolist(const char *, size_t, int *, char *, size_t *);
int alcove_decode_constant(const char *, size_t, int *, int *,
                           const alcove_constant_t *);
int alcove_decode_constant_list(const char *, size_t, int *, int *,
                                const alcove_constant_t *);
int alcove_decode_constant64(const char *buf, size_t len, int *index,
                             long long *val,
                             const alcove_constant_t *constants);
int alcove_decode_cstruct(const char *, size_t, int *, char *, size_t *,
                          alcove_alloc_t **, ssize_t *);
int alcove_decode_argv(const char *, size_t, int *, char ***);
void alcove_free_argv(char **);

int alcove_encode_version(char *, size_t, int *);
int alcove_encode_list_header(char *, size_t, int *, int);
int alcove_encode_empty_list(char *, size_t, int *);
int alcove_encode_tuple_header(char *, size_t, int *, int);
int alcove_encode_long(char *, size_t, int *, long);
int alcove_encode_ulong(char *, size_t, int *, unsigned long);
int alcove_encode_longlong(char *, size_t, int *, long long);
int alcove_encode_ulonglong(char *, size_t, int *, unsigned long long);
int alcove_encode_atom(char *, size_t, int *, const char *);
int alcove_encode_binary(char *, size_t, int *, const void *, long);
int alcove_encode_constant(char *, size_t, int *, char *,
                           const alcove_constant_t *);
int alcove_lookup_constant(char *, long long *, const alcove_constant_t *);
int alcove_encode_constant_id(char *, size_t, int *, long long,
                              const alcove_constant_t *);
int alcove_encode_cstruct(char *, size_t, int *, const char *, size_t,
                          alcove_alloc_t *, ssize_t);

ssize_t alcove_mk_errno(char *buf, size_t len, int errnum);
ssize_t alcove_mk_error(char *buf, size_t len, const char *reason);
ssize_t alcove_mk_atom(char *buf, size_t len, const char *atom);
ssize_t alcove_mk_binary(char *, size_t, const void *, size_t);
ssize_t alcove_mk_long(char *, size_t, long);
ssize_t alcove_mk_ulong(char *, size_t, unsigned long);

ssize_t alcove_call(alcove_state_t *ap, u_int32_t call, const char *arg,
                    size_t len, char *reply, size_t rlen);
