/* Copyright (c) 2018, Michael Santos <michael.santos@gmail.com>
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
  uint8_t j = 0;
  uint8_t k = 0;
  uint32_t nr = 0;

  /* call */
  if (alcove_decode_uint(arg, len, &index, &nr) < 0)
    return -1;

  if (nr >= ALCOVE_MAX_NR)
    return alcove_mk_errno(reply, rlen, EINVAL);

  j = nr / 8;
  k = nr % 8;

  ap->filter[j] |= (1 << k);

  return alcove_mk_atom(reply, rlen, "ok");
}
