/* Copyright (c) 2016, Michael Santos <michael.santos@gmail.com>
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

#if defined(__FreeBSD__)
#include "alcove_cap_constants.h"
#include <sys/capsicum.h>
#endif

/*
 * cap_fcntls_get(2)
 *
 */
ssize_t alcove_sys_cap_fcntls_get(alcove_state_t *ap, const char *arg,
                                  size_t len, char *reply, size_t rlen) {
#if defined(__FreeBSD__)
  int index = 0;
  int rindex = 0;
  int rv = 0;

  int fd = -1;
  uint32_t rights = 0;

  UNUSED(ap);

  /* fd */
  if (alcove_decode_int(arg, len, &index, &fd) < 0)
    return -1;

  rv = cap_fcntls_get(fd, &rights);

  if (rv < 0)
    return alcove_mk_errno(reply, rlen, errno);

  ALCOVE_OK(reply, rlen, &rindex,
            alcove_encode_ulong(reply, rlen, &rindex, rights));

  return rindex;
#else
  UNUSED(ap);
  UNUSED(arg);
  UNUSED(len);

  return alcove_mk_atom(reply, rlen, "undef");
#endif
}
