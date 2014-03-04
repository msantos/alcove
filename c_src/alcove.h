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
#define _GNU_SOURCE
#include <sched.h>

#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdbool.h>
#include <sys/param.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <signal.h>

#include <erl_driver.h>
#include <erl_interface.h>

#include "erl_errno.h"
#include "alcove_version.h"

enum {
    alcove_opt_daemonize = 1 << 0,           /* Run the process as a daemon */
    alcove_opt_closeallfds = 1 << 1,         /* Close fd's associated with the process */
};

typedef struct {
    u_int32_t opt;
    u_int8_t verbose;
    int fdin;
    int fdout;
    int fderr;
} alcove_state_t;

typedef struct {
    u_int32_t cmd;
    unsigned char *arg;
} alcove_msg_t;

void gotsig(int sig);

void alcove_ctl(alcove_state_t *ap);

void *alcove_malloc(ssize_t size);

ETERM *alcove_list_head(ETERM **, ETERM *);
ETERM *alcove_tuple2(ETERM *, ETERM *);
ETERM *alcove_tuple3(ETERM *, ETERM *, ETERM *);
ETERM *alcove_errno(int);
ETERM *alcove_error(const char *);
ETERM *alcove_ok(ETERM *);
ETERM *alcove_bool(bool);
ETERM *alcove_bin(const char *);

ETERM *alcove_cmd(alcove_state_t *, u_int32_t, ETERM *);

#define VERBOSE(x, ...) do { \
    if (ap->verbose >= x) { \
        erl_err_msg(__VA_ARGS__); \
    } \
} while (0)
