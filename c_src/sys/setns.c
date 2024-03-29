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
#ifdef __linux__
#define _GNU_SOURCE
#include <sched.h>
#ifndef HAVE_SETNS
#include <sys/syscall.h>
#endif
#endif

#include "alcove.h"
#include "alcove_call.h"

#include "alcove_fork.h"

#include "alcove_clone_constants.h"

/*
 * setns(2)
 *
 */
#ifdef __linux__
#ifndef HAVE_SETNS
int setns(int fd, int nstype) {
#ifdef SYS_setns
  return syscall(SYS_setns, fd, nstype);
#else
  errno = ENOSYS;
  return -1;
#endif
}
#endif
#endif

ssize_t alcove_sys_setns(alcove_state_t *ap, const char *arg, size_t len,
                         char *reply, size_t rlen) {
#ifdef __linux__
  int index = 0;

  int fd = -1;
  int nstype = 0;
  int rv;

  UNUSED(ap);

  /* file descriptor */
  if (alcove_decode_int(arg, len, &index, &fd) < 0)
    return -1;

  /* nstype */
  switch (alcove_decode_constant(arg, len, &index, &nstype,
                                 alcove_clone_constants)) {
  case 0:
    break;
  case 1:
    return alcove_mk_error(reply, rlen, "enotsup");
  default:
    return -1;
  }

  rv = setns(fd, nstype);

  return (rv < 0) ? alcove_mk_errno(reply, rlen, errno)
                  : alcove_mk_atom(reply, rlen, "ok");
#else
  UNUSED(ap);
  UNUSED(arg);
  UNUSED(len);

  return alcove_mk_atom(reply, rlen, "undef");
#endif
}
