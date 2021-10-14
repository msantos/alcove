/* Copyright (c) 2016-2021, Michael Santos <michael.santos@gmail.com>
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
 * pledge(2)
 *
 */
ssize_t alcove_sys_pledge(alcove_state_t *ap, const char *arg, size_t len,
                          char *reply, size_t rlen) {
#if defined(__OpenBSD__)
  int index = 0;
  int type = 0;
  int arity = 0;

  char promises[PATH_MAX] = {0};
  size_t plen = sizeof(promises) - 1;
  char execpromises[PATH_MAX] = {0};
  size_t elen = sizeof(execpromises) - 1;
  int rv = 0;

  UNUSED(ap);

  /* promises */
  if (alcove_get_type(arg, len, &index, &type, &arity) < 0)
    return -1;

  switch (type) {
  case ERL_NIL_EXT:
    if (alcove_decode_list_header(arg, len, &index, &arity) < 0 || arity != 0)
      return -1;

    plen = -1;
    break;

  case ERL_ATOM_EXT:
    if (alcove_decode_null(arg, len, &index) < 0)
      return -1;

    plen = -1;
    break;

  default:
    if (alcove_decode_iolist(arg, len, &index, promises, &plen) < 0)
      return -1;
  }

  /* execpromises */
  if (alcove_get_type(arg, len, &index, &type, &arity) < 0)
    return -1;

  switch (type) {
  case ERL_NIL_EXT:
    if (alcove_decode_list_header(arg, len, &index, &arity) < 0 || arity != 0)
      return -1;

    elen = -1;
    break;

  case ERL_ATOM_EXT:
    if (alcove_decode_null(arg, len, &index) < 0)
      return -1;

    elen = -1;
    break;

  default:
    if (alcove_decode_iolist(arg, len, &index, execpromises, &elen) < 0)
      return -1;
  }

  rv = pledge(plen == -1 ? NULL : promises, elen == -1 ? NULL : execpromises);

  return (rv < 0) ? alcove_mk_errno(reply, rlen, errno)
                  : alcove_mk_atom(reply, rlen, "ok");
#else
  UNUSED(ap);
  UNUSED(arg);
  UNUSED(len);

  return alcove_mk_atom(reply, rlen, "undef");
#endif
}
