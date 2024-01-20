/* Copyright (c) 2015-2024, Michael Santos <michael.santos@gmail.com>
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

int pid_foreach(alcove_state_t *ap, pid_t pid, void *arg1, void *arg2,
                int (*comp)(pid_t, pid_t),
                int (*fp)(alcove_state_t *ap, alcove_child_t *, void *,
                          void *)) {
  int i;
  int rv;

  UNUSED(ap);

  for (i = 0; i < ap->fdsetsize; i++) {
    if ((*comp)(ap->child[i].pid, pid) == 0)
      continue;

    rv = (*fp)(ap, &(ap->child[i]), arg1, arg2);

    if (rv <= 0)
      return rv;
  }

  return 1;
}

int pid_equal(pid_t p1, pid_t p2) { return p1 == p2; }

int pid_not_equal(pid_t p1, pid_t p2) { return p1 != p2; }

int fdlimit_pid(alcove_state_t *ap, alcove_child_t *c, void *arg1, void *arg2) {
  int *n = arg1;
  int *maxfd = arg2;

  if (*maxfd < c->fdctl || *maxfd < c->fdout || *maxfd < c->fderr ||
      *maxfd < c->fdin)
    return -1;

  *n += 1;

  return 1;
}
