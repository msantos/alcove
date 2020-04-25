/* Copyright (c) 2016-2017, Michael Santos <michael.santos@gmail.com>
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
#include <sys/capsicum.h>

#include "alcove_ioctl_constants.h"

static int alcove_decode_cap_ioctls_list(const char *buf, size_t len,
                                         int *index, unsigned long *cmds,
                                         size_t *ncmds,
                                         const alcove_constant_t *constants);
#endif

/*
 * cap_ioctls_limit(2)
 *
 */
ssize_t alcove_sys_cap_ioctls_limit(alcove_state_t *ap, const char *arg,
                                    size_t len, char *reply, size_t rlen) {
#if defined(__FreeBSD__)
  int index = 0;
  int rv;

  int fd;
  unsigned long cmds[256] = {0};
  size_t ncmds = sizeof(cmds);

  UNUSED(ap);

  /* fd */
  if (alcove_decode_int(arg, len, &index, &fd) < 0)
    return -1;

  /* ioctls */
  switch (alcove_decode_cap_ioctls_list(arg, len, &index, cmds, &ncmds,
                                        alcove_ioctl_constants)) {
  case 0:
    break;
  case 1:
    return alcove_mk_error(reply, rlen, "enotsup");
  default:
    return -1;
  }

  rv = cap_ioctls_limit(fd, cmds, ncmds);

  return (rv < 0) ? alcove_mk_errno(reply, rlen, errno)
                  : alcove_mk_atom(reply, rlen, "ok");
#else
  UNUSED(ap);
  UNUSED(arg);
  UNUSED(len);

  return alcove_mk_atom(reply, rlen, "undef");
#endif
}

#if defined(__FreeBSD__)
static int alcove_decode_cap_ioctls_list(const char *buf, size_t len,
                                         int *index, unsigned long *cmds,
                                         size_t *ncmds,
                                         const alcove_constant_t *constants) {
  int type = 0;
  int arity = 0;
  size_t n = 0;

  if (alcove_get_type(buf, len, index, &type, &arity) < 0)
    return -1;

  if (arity > *ncmds)
    return -1;

  switch (type) {
  case ERL_NIL_EXT:
    if (ei_decode_list_header(buf, index, &arity) < 0)
      return -1;
    break;

  case ERL_SMALL_INTEGER_EXT:
  case ERL_INTEGER_EXT: {
    unsigned long val = 0;

    if (alcove_decode_ulong(buf, len, index, &val) < 0)
      return -1;

    cmds[n++] = val;
  } break;

  case ERL_STRING_EXT: {
    char tmp[MAXMSGLEN] = {0};
    char *p = tmp;

    if (arity >= sizeof(tmp))
      return -1;

    if (ei_decode_string(buf, index, tmp) < 0)
      return -1;

    for (; *p; p++)
      cmds[n++] = *p;
  } break;

  case ERL_LIST_EXT: {
    int i = 0;
    int length = 0;
    long long constant = 0;
    int rv = 0;

    if (ei_decode_list_header(buf, index, &length) < 0)
      return -1;

    for (i = 0; i < length; i++) {
      rv = alcove_decode_constant64(buf, len, index, &constant, constants);

      if (rv != 0)
        return rv;

      if (constant < 0 || constant > UINT32_MAX)
        return -1;

      cmds[n++] = constant;
    }

    /* [] */
    if (alcove_decode_list_header(buf, len, index, &length) < 0 || length != 0)
      return -1;

  } break;

  default:
    return -1;
  }

  *ncmds = n;

  return 0;
}
#endif
