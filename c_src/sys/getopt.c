/* Copyright (c) 2014-2018, Michael Santos <michael.santos@gmail.com>
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

#define ALCOVE_GETOPT_NOTFOUND -2

/* Get port options */
ssize_t alcove_sys_getopt(alcove_state_t *ap, const char *arg, size_t len,
                          char *reply, size_t rlen) {
  int index = 0;
  char opt[MAXATOMLEN] = {0};
  int val = ALCOVE_GETOPT_NOTFOUND;

  /* opt */
  if (alcove_decode_atom(arg, len, &index, opt) < 0)
    return -1;

  if (strcmp(opt, "exit_status") == 0) {
    val = ap->opt & alcove_opt_exit_status ? 1 : 0;
  }
  if (strcmp(opt, "flowcontrol") == 0) {
    val = ap->flowcontrol;
  } else if (strcmp(opt, "maxchild") == 0) {
    val = ap->maxchild;
  } else if (strcmp(opt, "maxforkdepth") == 0) {
    val = ap->maxforkdepth;
  } else if (strcmp(opt, "termsig") == 0) {
    val = ap->opt & alcove_opt_termsig ? 1 : 0;
  } else if (strcmp(opt, "signaloneof") == 0) {
    val = ap->signaloneof;
  } else if (strcmp(opt, "stdin_closed") == 0) {
    val = ap->opt & alcove_opt_stdin_closed ? 1 : 0;
  } else if (strcmp(opt, "stdout_closed") == 0) {
    val = ap->opt & alcove_opt_stdout_closed ? 1 : 0;
  } else if (strcmp(opt, "stderr_closed") == 0) {
    val = ap->opt & alcove_opt_stderr_closed ? 1 : 0;
  }

  return (val == ALCOVE_GETOPT_NOTFOUND) ? alcove_mk_atom(reply, rlen, "false")
                                         : alcove_mk_long(reply, rlen, val);
}
