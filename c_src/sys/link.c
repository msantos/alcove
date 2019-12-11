/* Copyright (c) 2015, Michael Santos <michael.santos@gmail.com>
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

/*
 * link(2)
 *
 */
ssize_t alcove_sys_link(alcove_state_t *ap, const char *arg, size_t len,
                        char *reply, size_t rlen) {
  int index = 0;
  char oldpath[PATH_MAX] = {0};
  char newpath[PATH_MAX] = {0};
  size_t oldpathlen = sizeof(oldpath) - 1;
  size_t newpathlen = sizeof(newpath) - 1;
  int rv = 0;

  UNUSED(ap);

  /* oldpath */
  if (alcove_decode_iolist(arg, len, &index, oldpath, &oldpathlen) < 0 ||
      oldpathlen == 0)
    return -1;

  /* newpath */
  if (alcove_decode_iolist(arg, len, &index, newpath, &newpathlen) < 0 ||
      newpathlen == 0)
    return -1;

  rv = link(oldpath, newpath);

  return (rv < 0) ? alcove_mk_errno(reply, rlen, errno)
                  : alcove_mk_atom(reply, rlen, "ok");
}
