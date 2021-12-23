/* Copyright (c) 2014-2021, Michael Santos <michael.santos@gmail.com>
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

#include "alcove_signal_constants.h"

static int atom_to_sighandler(struct sigaction *act, int signum, char *handler);
static char *sighandler_to_atom(struct sigaction *act);

/*
 * sigaction(2)
 *
 */
ssize_t alcove_sys_sigaction(alcove_state_t *ap, const char *arg, size_t len,
                             char *reply, size_t rlen) {
  int index = 0;
  int rindex = 0;
  int type = 0;
  int arity = 0;

  int signum = 0;
  char handler[MAXATOMLEN] = {0};
  char *ohandler = NULL;
  struct sigaction *pact = NULL;
  struct sigaction act = {0};
  struct sigaction oact = {0};

  /* signum */
  switch (alcove_decode_constant(arg, len, &index, &signum,
                                 alcove_signal_constants)) {
  case 0:
    break;
  case 1:
    return alcove_mk_error(reply, rlen, "enotsup");
  default:
    return -1;
  }

  /* handler */
  if (alcove_get_type(arg, len, &index, &type, &arity) < 0)
    return -1;

  switch (type) {
  case ERL_NIL_EXT:
    if (alcove_decode_list_header(arg, len, &index, &arity) < 0 || arity != 0)
      return -1;

    break;

  case ERL_BINARY_EXT:
    if (arity > 0)
      return -1;

    break;

  default:
    if (alcove_decode_atom(arg, len, &index, handler) < 0)
      return -1;

    if (atom_to_sighandler(&act, signum, handler) < 0)
      return -1;

    act.sa_flags |= SA_SIGINFO;
    (void)sigfillset(&act.sa_mask);

    pact = &act;
  }

  /* SIGCHLD handling */
  if (signum == SIGCHLD) {
    ohandler = ap->sigchld ? "sig_info" : "sig_dfl";

    if (pact == NULL)
      goto REPLY;

    ap->sigchld = (pact->sa_sigaction == alcove_sig_info) ? 1 : 0;

    goto REPLY;
  }

  if (sigaction(signum, pact, &oact) < 0)
    alcove_mk_errno(reply, rlen, errno);

  ohandler = sighandler_to_atom(&oact);

  /* Unknown signal handler installed: crash process */
  if (ohandler == NULL)
    abort();

REPLY:
  ALCOVE_ERR(alcove_encode_version(reply, rlen, &rindex));
  ALCOVE_ERR(alcove_encode_tuple_header(reply, rlen, &rindex, 2));
  ALCOVE_ERR(alcove_encode_atom(reply, rlen, &rindex, "ok"));
  ALCOVE_ERR(alcove_encode_atom(reply, rlen, &rindex, ohandler));

  return rindex;
}

static int atom_to_sighandler(struct sigaction *act, int signum,
                              char *handler) {
  if (strcmp(handler, "sig_info") == 0) {
    act->sa_sigaction = alcove_sig_info;
  } else if (strcmp(handler, "sig_dfl") == 0) {
    act->sa_handler = SIG_DFL;
  } else if (strcmp(handler, "sig_ign") == 0) {
    act->sa_handler = SIG_IGN;
  } else {
    return -1;
  }

  return 0;
}

static char *sighandler_to_atom(struct sigaction *act) {
  if (act->sa_sigaction == alcove_sig_info) {
    return "sig_info";
  } else if (act->sa_handler == SIG_DFL) {
    return "sig_dfl";
  } else if (act->sa_handler == SIG_IGN) {
    return "sig_ign";
  }

  return NULL;
}
