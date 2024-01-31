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

#include <sys/mount.h>

#include "alcove_mount_constants.h"

/*
 * umount2(2)
 *
 */
ssize_t alcove_sys_umount2(alcove_state_t *ap, const char *arg, size_t len,
                           char *reply, size_t rlen) {
#if defined(__sunos__)
  UNUSED(ap);
  UNUSED(arg);
  UNUSED(len);

  return alcove_mk_atom(reply, rlen, "undef");
#else
  int index = 0;

  char source[PATH_MAX] = {0};
  size_t slen = sizeof(source) - 1;
  unsigned long flags;
  int val = 0;

  int rv;

  UNUSED(ap);

  /* source */
  if (alcove_decode_iolist(arg, len, &index, source, &slen) < 0 || slen == 0)
    return -1;

  /* mountflags */
  switch (alcove_decode_constant_list(arg, len, &index, &val,
                                      alcove_mount_constants)) {
  case 0:
    if (val < 0)
      return -1;

    flags = val;
    break;
  case 1:
    return alcove_mk_error(reply, rlen, "enotsup");
  default:
    return -1;
  }

#if defined(__linux__) || defined(__sunos__)
  rv = umount2(source, flags);
#else
  rv = unmount(source, flags);
#endif

  return (rv < 0) ? alcove_mk_errno(reply, rlen, errno)
                  : alcove_mk_atom(reply, rlen, "ok");
#endif
}
