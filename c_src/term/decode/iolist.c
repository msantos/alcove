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

static int alcove_decode_iolist_internal(const char *buf, size_t len,
                                         int *index, char *res, size_t rlen,
                                         int *rindex, int depth);

int alcove_decode_iolist(const char *buf, size_t len, int *index, char *res,
                         size_t *rlen) {
  int type = 0;
  int arity = 0;
  int rindex = 0;

  if (alcove_get_type(buf, len, index, &type, &arity) < 0)
    return -1;

  switch (type) {
  case ERL_BINARY_EXT:
  case ERL_LIST_EXT:
  case ERL_NIL_EXT:
  case ERL_STRING_EXT:
    break;

  default:
    return -1;
  }

  if (alcove_decode_iolist_internal(buf, len, index, res, *rlen, &rindex, 0) <
      0)
    return -1;

  *rlen = rindex;

  return 0;
}

static int alcove_decode_iolist_internal(const char *buf, size_t len,
                                         int *index, char *res, size_t rlen,
                                         int *rindex, int depth) {
  int type = 0;
  int arity = 0;

  /* Arbitrary depth to avoid stack overflows */
  if (depth > 16)
    return -1;

  if (alcove_get_type(buf, len, index, &type, &arity) < 0)
    return -1;

  switch (type) {
  case ERL_STRING_EXT:
    if (*rindex + arity + 1 > rlen)
      return -1;

    if (ei_decode_string(buf, index, res + *rindex) < 0)
      return -1;

    /* Do not include trailing NULL */
    *rindex += arity;
    break;

  case ERL_BINARY_EXT: {
    long int length = 0;

    if (*rindex + arity > rlen)
      return -1;

    if (ei_decode_binary(buf, index, res + *rindex, &length) < 0)
      return -1;

    *rindex += length;
  }

  break;

  case ERL_SMALL_INTEGER_EXT: {
    unsigned long p = 0;

    if (*rindex + 1 > rlen)
      return -1;

    if (ei_decode_ulong(buf, index, &p) < 0)
      return -1;

    res[*rindex] = p;
    *rindex += 1;
  }

  break;

  case ERL_NIL_EXT:
    if (ei_decode_list_header(buf, index, &arity) < 0)
      return -1;
    break;

  case ERL_LIST_EXT: {
    int i;
    int length = 0;

    if (ei_decode_list_header(buf, index, &length) < 0)
      return -1;

    for (i = 0; i < length; i++) {
      if (*rindex >= rlen)
        return -1;
      if (alcove_decode_iolist_internal(buf, len, index, res, rlen, rindex,
                                        depth + 1) < 0)
        return -1;
    }

    /* [] */
    if (alcove_decode_list_header(buf, len, index, &length) < 0 || length != 0)
      return -1;

  }

  break;

  default:
    return -1;
  }

  return 0;
}
