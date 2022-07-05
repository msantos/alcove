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
#include "alcove.h"
#include "alcove_call.h"

/* Set port options */
ssize_t alcove_sys_setopt(alcove_state_t *ap, const char *arg, size_t len,
                          char *reply, size_t rlen) {
  int index = 0;
  char opt[MAXATOMLEN] = {0};
  int32_t val = 0;

  /* opt */
  if (alcove_decode_atom(arg, len, &index, opt) < 0)
    return -1;

  /* val */
  if (alcove_decode_int(arg, len, &index, &val) < 0)
    return -1;

  if (strcmp(opt, "flowcontrol") == 0) {
    ap->flowcontrol = val < 0 ? -1 : val;
    return alcove_mk_atom(reply, rlen, "true");
  }

  if (val < 0)
    return -1;

  if (strcmp(opt, "exit_status") == 0) {
    ALCOVE_SETOPT(ap, alcove_opt_exit_status, val);
  } else if (strcmp(opt, "maxchild") == 0) {
    size_t nproc = 0;
    struct rlimit maxfd = {0};

    val = MIN(val, UINT16_MAX);

    if (getrlimit(RLIMIT_NOFILE, &maxfd) < 0)
      return alcove_mk_atom(reply, rlen, "false");

    /* Count the number of PIDs and test existing fd's are below the fd limit */
    if (pid_foreach(ap, 0, &nproc, &maxfd, pid_not_equal, fdlimit_pid) < 0)
      return alcove_mk_atom(reply, rlen, "false");

    if (nproc > val)
      return alcove_mk_atom(reply, rlen, "false");

    if (ALCOVE_NFD(val) > maxfd.rlim_cur) {
      return alcove_mk_atom(reply, rlen, "false");
    }

    ap->maxchild = val;
  } else if (strcmp(opt, "maxforkdepth") == 0) {
    ap->maxforkdepth = MIN(val, UINT8_MAX);
  } else if (strcmp(opt, "termsig") == 0) {
    ALCOVE_SETOPT(ap, alcove_opt_termsig, val);
  } else if (strcmp(opt, "signaloneof") == 0) {
    ap->signaloneof = MIN(val, UINT8_MAX);
  } else if (strcmp(opt, "stdin_closed") == 0) {
    ALCOVE_SETOPT(ap, alcove_opt_stdin_closed, val);
  } else if (strcmp(opt, "stdout_closed") == 0) {
    ALCOVE_SETOPT(ap, alcove_opt_stdout_closed, val);
  } else if (strcmp(opt, "stderr_closed") == 0) {
    ALCOVE_SETOPT(ap, alcove_opt_stderr_closed, val);
  } else {
    return alcove_mk_atom(reply, rlen, "false");
  }

  return alcove_mk_atom(reply, rlen, "true");
}
