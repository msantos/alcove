/* Copyright (c) 2014, Michael Santos <michael.santos@gmail.com>
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

#include "alcove_fcntl_constants.h"

/*
 * fcntl constants
 *
 */
ssize_t alcove_sys_fcntl_constant(alcove_state_t *ap, const char *arg,
                                  size_t len, char *reply, size_t rlen) {
  int index = 0;
  int rindex = 0;

  char name[MAXATOMLEN] = {0};

  UNUSED(ap);

  /* constant */
  if (alcove_decode_atom(arg, len, &index, name) < 0)
    return -1;

  ALCOVE_ERR(alcove_encode_version(reply, rlen, &rindex));
  ALCOVE_ERR(alcove_encode_constant(reply, rlen, &rindex, name,
                                    alcove_fcntl_constants));

  return rindex;
}
