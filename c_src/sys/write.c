/* Copyright (c) 2014-2024, Michael Santos <michael.santos@gmail.com>
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
 * write(2)
 *
 */
ssize_t alcove_sys_write(alcove_state_t *ap, const char *arg, size_t len,
                         char *reply, size_t rlen) {
  int index = 0;
  int rindex = 0;

  int fd = -1;
  char buf[MAXMSGLEN] = {0};
  size_t buflen = sizeof(buf);
  int rv;

  UNUSED(ap);

  /* fd */
  if (alcove_decode_int(arg, len, &index, &fd) < 0)
    return -1;

  /* buf */
  if (alcove_decode_iolist(arg, len, &index, buf, &buflen) < 0)
    return -1;

  rv = write(fd, buf, buflen);

  if (rv < 0) {
    rindex = alcove_mk_errno(reply, rlen, errno);
  } else {
    ALCOVE_OK(reply, rlen, &rindex,
              alcove_encode_longlong(reply, rlen, &rindex, rv));
  }

  return rindex;
}
