/* Copyright (c) 2018-2024, Michael Santos <michael.santos@gmail.com>
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

static int set_flowcontrol_pid(alcove_state_t *ap, alcove_child_t *c,
                               void *arg1, void *arg2);
static int set_signaloneof_pid(alcove_state_t *ap, alcove_child_t *c,
                               void *arg1, void *arg2);

/* Set port options */
ssize_t alcove_sys_setcpid(alcove_state_t *ap, const char *arg, size_t len,
                           char *reply, size_t rlen) {
  int index = 0;
  char opt[MAXATOMLEN] = {0};
  u_int32_t pid = 0;
  int32_t val = 0;

  /* pid */
  if (alcove_decode_uint(arg, len, &index, &pid) < 0)
    return -1;

  /* opt */
  if (alcove_decode_atom(arg, len, &index, opt) < 0)
    return -1;

  /* val */
  if (alcove_decode_int(arg, len, &index, &val) < 0)
    return -1;

  if (strcmp(opt, "flowcontrol") == 0) {
    val = val < 0 ? -1 : val;
    (void)pid_foreach(ap, pid, &val, NULL, pid_equal, set_flowcontrol_pid);
    return alcove_mk_atom(reply, rlen, "true");
  }

  if (val < 0)
    return -1;

  if (strcmp(opt, "signaloneof") == 0) {
    val = MIN(val, UINT8_MAX);
    (void)pid_foreach(ap, pid, &val, NULL, pid_equal, set_signaloneof_pid);
  } else {
    return alcove_mk_atom(reply, rlen, "false");
  }

  return alcove_mk_atom(reply, rlen, "true");
}

static int set_flowcontrol_pid(alcove_state_t *ap, alcove_child_t *c,
                               void *arg1, void *arg2) {
  int32_t *count = arg1;

  UNUSED(ap);
  UNUSED(arg2);

  c->flowcontrol = *count;

  return 0;
}

static int set_signaloneof_pid(alcove_state_t *ap, alcove_child_t *c,
                               void *arg1, void *arg2) {
  int *sig = arg1;

  UNUSED(ap);
  UNUSED(arg2);

  c->signaloneof = *sig;

  return 0;
}
