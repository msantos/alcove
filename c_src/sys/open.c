/* Copyright (c) 2014-2017, Michael Santos <michael.santos@gmail.com>
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

#include "alcove_file_constants.h"

#include <sys/stat.h>

#include <fcntl.h>

/*
 * open(2)
 *
 */
ssize_t alcove_sys_open(alcove_state_t *ap, const char *arg, size_t len,
                        char *reply, size_t rlen) {
  int index = 0;
  int rindex = 0;
  char pathname[PATH_MAX] = {0};
  size_t plen = sizeof(pathname) - 1;
  int flags = 0;
  mode_t mode = {0};
  int fd = 0;

  u_int32_t val = 0;

  UNUSED(ap);

  /* pathname */
  if (alcove_decode_iolist(arg, len, &index, pathname, &plen) < 0 || plen == 0)
    return -1;

  /* flags */
  switch (alcove_decode_constant_list(arg, len, &index, &flags,
                                      alcove_file_constants)) {
  case 0:
    break;
  case 1:
    return alcove_mk_error(reply, rlen, "enotsup");
  default:
    return -1;
  }

  /* mode */
  if (alcove_decode_uint(arg, len, &index, &val) < 0)
    return -1;

  mode = val;

  fd = open(pathname, flags, mode);

  if (fd < 0)
    return alcove_mk_errno(reply, rlen, errno);

  ALCOVE_OK(reply, rlen, &rindex, alcove_encode_long(reply, rlen, &rindex, fd));

  return rindex;
}
