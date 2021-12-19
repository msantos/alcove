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

static void set_filter(uint8_t *filter, const char *calls, int arity);

/* Allow/filter calls */
ssize_t alcove_sys_filter(alcove_state_t *ap, const char *arg, size_t len,
                          char *reply, size_t rlen) {
  int index = 0;
  int type = 0;
  int arity = 0;
  int arity1 = 0;

  char calls[ALCOVE_NR_SIZE] = {0};
  char calls1[ALCOVE_NR_SIZE] = {0};

  size_t csize = 0;
  size_t csize1 = 0;

  /* calls: process */
  if (alcove_get_type(arg, len, &index, &type, &arity) < 0)
    return -1;

  switch (type) {
  case ERL_NIL_EXT:
    if (alcove_decode_list_header(arg, len, &index, &arity) < 0 || arity != 0)
      return -1;

    break;

  case ERL_BINARY_EXT:
    if (arity > ALCOVE_NR_SIZE)
      return -1;

    if (alcove_decode_binary(arg, len, &index, calls, &csize) < 0)
      return -1;

    break;

  default:
    return -1;
  }

  /* calls: subprocess */
  if (alcove_get_type(arg, len, &index, &type, &arity1) < 0)
    return -1;

  switch (type) {
  case ERL_NIL_EXT:
    if (alcove_decode_list_header(arg, len, &index, &arity1) < 0 || arity1 != 0)
      return -1;

    break;

  case ERL_BINARY_EXT:
    if (arity1 > ALCOVE_NR_SIZE)
      return -1;

    if (alcove_decode_binary(arg, len, &index, calls1, &csize1) < 0)
      return -1;

    break;

  default:
    return -1;
  }

  set_filter(ap->filter, calls, arity);
  set_filter(ap->filter1, calls1, arity1);

  return alcove_mk_atom(reply, rlen, "ok");
}

static void set_filter(uint8_t *filter, const char *calls, int arity) {
  int n;

  for (n = 0; n < arity; n++) {
    filter[n] |= calls[n];
  }
}
