/* Copyright (c) 2021, Michael Santos <michael.santos@gmail.com>
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

#ifdef __FreeBSD__
#include <sys/procctl.h>
#endif

#include "alcove_procctl_constants.h"
#include "alcove_wait_constants.h"

/*
 * procctl(2)
 *
 */
ssize_t alcove_sys_procctl(alcove_state_t *ap, const char *arg, size_t len,
                           char *reply, size_t rlen) {
#ifdef __FreeBSD__
  int index = 0;
  int rindex;
  int type = 0;
  int arity = 0;

  idtype_t idtype;
  id_t id;
  int cmd;
  char data[MAXMSGLEN] = {0};
  size_t datalen = sizeof(data);
  alcove_alloc_t *elem = NULL;
  ssize_t nelem = 0;

  int rv = 0;

  UNUSED(ap);

  /* idtype */
  switch (alcove_decode_constant(arg, len, &index, (int *)&idtype,
                                 alcove_wait_constants)) {
  case 0:
    break;
  case 1:
    return alcove_mk_error(reply, rlen, "enotsup");
  default:
    return -1;
  }

  /* id */
  if (alcove_decode_long(arg, len, &index, &id) < 0)
    return -1;

  /* cmd */
  switch (alcove_decode_constant(arg, len, &index, &cmd,
                                 alcove_procctl_constants)) {
  case 0:
    break;
  case 1:
    return alcove_mk_error(reply, rlen, "enotsup");
  default:
    return -1;
  }

  /* data */
  if (alcove_get_type(arg, len, &index, &type, &arity) < 0)
    return -1;

  switch (type) {
    case ERL_NIL_EXT:
      datalen = 0;
      break;

    case ERL_LIST_EXT:
      if (alcove_decode_cstruct(arg, len, &index, data, &datalen, &elem, &nelem) < 0)
        return -1;
      break;

    default:
      return -1;
  }

  rv = procctl(idtype, id, cmd, datalen > 0 ? data : NULL);

  if (rv < 0)
    return alcove_mk_errno(reply, rlen, errno);

  ALCOVE_TUPLE3(
      reply, rlen, &rindex, "ok",
      alcove_encode_binary(reply, rlen, &rindex, data, datalen),
      alcove_encode_cstruct(reply, rlen, &rindex, data, datalen, elem, nelem));

  return rindex;
#else
  UNUSED(ap);
  UNUSED(arg);
  UNUSED(len);

  return alcove_mk_atom(reply, rlen, "undef");
#endif
}
