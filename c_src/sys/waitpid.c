/* Copyright (c) 2016-2024, Michael Santos <michael.santos@gmail.com>
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

#include "alcove_wait_constants.h"

#include <sys/wait.h>

static int remove_pid(alcove_state_t *ap, alcove_child_t *c, void *arg1,
                      void *arg2);

/*
 * waitpid(2)
 *
 */
ssize_t alcove_sys_waitpid(alcove_state_t *ap, const char *arg, size_t len,
                           char *reply, size_t rlen) {
  int index = 0;
  int rindex = 0;

  pid_t pid = 0;
  int status = 0;
  int options = 0;

  pid_t rv;

  UNUSED(ap);

  /* pid */
  if (alcove_decode_int(arg, len, &index, &pid) < 0)
    return -1;

  /* options */
  switch (alcove_decode_constant_list(arg, len, &index, &options,
                                      alcove_wait_constants)) {
  case 0:
    break;
  case 1:
    return alcove_mk_error(reply, rlen, "enotsup");
  default:
    return -1;
  }

  rv = waitpid(pid, &status, options);

  if (rv < 0)
    return alcove_mk_errno(reply, rlen, errno);

  if (WIFEXITED(status) || WIFSIGNALED(status)) {
    if (pid_foreach(ap, rv, NULL, NULL, pid_equal, remove_pid) < 0)
      return -1;
  }

  ALCOVE_ERR(alcove_encode_version(reply, rlen, &rindex));
  ALCOVE_ERR(alcove_encode_tuple_header(reply, rlen, &rindex, 4));
  ALCOVE_ERR(alcove_encode_atom(reply, rlen, &rindex, "ok"));
  ALCOVE_ERR(alcove_encode_long(reply, rlen, &rindex, rv));
  ALCOVE_ERR(alcove_encode_long(reply, rlen, &rindex, status));

  if (rv) {
    if (WIFEXITED(status)) {
      ALCOVE_ERR(alcove_encode_list_header(reply, rlen, &rindex, 1));
      ALCOVE_ERR(alcove_encode_tuple_header(reply, rlen, &rindex, 2));
      ALCOVE_ERR(alcove_encode_atom(reply, rlen, &rindex, "exit_status"));
      ALCOVE_ERR(alcove_encode_long(reply, rlen, &rindex, WEXITSTATUS(status)));
    }

    if (WIFSIGNALED(status)) {
      ALCOVE_ERR(alcove_encode_list_header(reply, rlen, &rindex, 1));
      ALCOVE_ERR(alcove_encode_tuple_header(reply, rlen, &rindex, 2));
      ALCOVE_ERR(alcove_encode_atom(reply, rlen, &rindex, "termsig"));
      ALCOVE_ERR(alcove_signal_name(reply, rlen, &rindex, WTERMSIG(status)));
    }

    if (WIFSTOPPED(status)) {
      ALCOVE_ERR(alcove_encode_list_header(reply, rlen, &rindex, 1));
      ALCOVE_ERR(alcove_encode_tuple_header(reply, rlen, &rindex, 2));
      ALCOVE_ERR(alcove_encode_atom(reply, rlen, &rindex, "stopsig"));
      ALCOVE_ERR(alcove_signal_name(reply, rlen, &rindex, WSTOPSIG(status)));
    }

    if (WIFCONTINUED(status)) {
      ALCOVE_ERR(alcove_encode_list_header(reply, rlen, &rindex, 1));
      ALCOVE_ERR(alcove_encode_atom(reply, rlen, &rindex, "continued"));
    }
  }

  ALCOVE_ERR(alcove_encode_empty_list(reply, rlen, &rindex));

  return rindex;
}

static int remove_pid(alcove_state_t *ap, alcove_child_t *c, void *arg1,
                      void *arg2) {
  UNUSED(ap);
  UNUSED(arg1);
  UNUSED(arg2);

  c->pid = 0;
  c->fdctl = -1;
  c->fdin = -1;
  c->fdout = -1;
  c->fderr = -1;

  return 0;
}
