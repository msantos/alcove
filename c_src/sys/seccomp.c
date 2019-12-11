/* Copyright (c) 2017, Michael Santos <michael.santos@gmail.com>
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

#ifndef HAVE_SECCOMP
#ifdef __linux__
#include <syscall.h>
#endif
#endif

#ifdef __linux__
#ifdef HAVE_PRCTL_SECCOMP
#include <linux/audit.h>
#include <linux/filter.h>
#include <linux/seccomp.h>
#include <sys/ptrace.h>
#endif
#endif

#include "alcove_seccomp_constants.h"

typedef struct {
  char data[MAXMSGLEN];
  size_t len;
} alcove_seccomp_arg_t;

#ifdef __linux__
#ifndef HAVE_SECCOMP
static int seccomp(unsigned int operation, unsigned int flags, void *args);
#endif
#endif

/*
 * seccomp(2)
 *
 */
#ifdef __linux__
#ifndef HAVE_SECCOMP
static int seccomp(unsigned int operation, unsigned int flags, void *args) {
#ifdef __NR_seccomp
  return syscall(__NR_seccomp, operation, flags, args);
#else
#pragma message "No support for seccomp(2)"
  errno = ENOSYS;
  return -1;
#endif
}
#endif
#endif

ssize_t alcove_sys_seccomp(alcove_state_t *ap, const char *arg, size_t len,
                           char *reply, size_t rlen) {
#ifdef __linux__
  int index = 0;
  int type = 0;
  int arity = 0;

  unsigned int operation = 0;
  unsigned int flags = 0;
  alcove_alloc_t *elem = NULL;
  ssize_t nelem = 0;

  alcove_seccomp_arg_t args = {0};

  int rv = 0;

  UNUSED(ap);

  /* operation */
  switch (alcove_decode_constant(arg, len, &index, (int *)&operation,
                                 alcove_seccomp_constants)) {
  case 0:
    break;
  case 1:
    return alcove_mk_error(reply, rlen, "enotsup");
  default:
    return -1;
  }

  /* flags */
  switch (alcove_decode_constant(arg, len, &index, (int *)&flags,
                                 alcove_seccomp_constants)) {
  case 0:
    break;
  case 1:
    return alcove_mk_error(reply, rlen, "enotsup");
  default:
    return -1;
  }

  /* args */
  if (alcove_get_type(arg, len, &index, &type, &arity) < 0)
    return -1;

  switch (type) {
  case ERL_LIST_EXT:
    args.len = sizeof(args.data);
    if (alcove_decode_cstruct(arg, len, &index, args.data, &(args.len), &elem,
                              &nelem) < 0)
      return -1;

    break;

  case ERL_NIL_EXT:
    if (alcove_decode_list_header(arg, len, &index, &arity) < 0 || arity != 0)
      return -1;

    break;

  default:
    return -1;
  }

  rv = seccomp(operation, flags, args.len == 0 ? NULL : args.data);

  return (rv < 0) ? alcove_mk_errno(reply, rlen, errno)
                  : alcove_mk_atom(reply, rlen, "ok");
#else
  UNUSED(ap);
  UNUSED(arg);
  UNUSED(len);

  return alcove_mk_atom(reply, rlen, "undef");
#endif
}
