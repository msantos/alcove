/* Copyright (c) 2018-2021, Michael Santos <michael.santos@gmail.com>
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

/* Allow/filter calls */
ssize_t alcove_sys_filter(alcove_state_t *ap, const char *arg, size_t len,
                          char *reply, size_t rlen) {
  int index = 0;
  int type = 0;
  int arity = 0;
  int n;

  char *calls;
  uint8_t j = 0;
  uint8_t k = 0;
  uint32_t nr = 0;

  /* calls */
  if (alcove_get_type(arg, len, &index, &type, &arity) < 0)
    return -1;

  switch (type) {
  case ERL_NIL_EXT:
    return alcove_mk_atom(reply, rlen, "ok");

  case ERL_LIST_EXT:
    /* list element exceeds 255 */
    return alcove_mk_errno(reply, rlen, EINVAL);

  case ERL_STRING_EXT:
    break;

  default:
    return -1;
  }

  calls = calloc(arity + 1, 1);
  if (calls == NULL)
    return alcove_mk_errno(reply, rlen, ENOMEM);

  if (alcove_decode_string(arg, len, &index, calls, arity + 1) < 0) {
    free(calls);
    return -1;
  }

  for (n = 0; n < arity; n++) {
    nr = calls[n];

    if (nr >= ALCOVE_MAX_NR) {
      free(calls);
      return alcove_mk_errno(reply, rlen, EINVAL);
    }
  }

  for (n = 0; n < arity; n++) {
    nr = calls[n];

    j = nr / 8;
    k = nr % 8;

    ap->filter[j] |= (1 << k);
  }

  free(calls);

  return alcove_mk_atom(reply, rlen, "ok");
}
