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
static const alcove_constant_t alcove_procctl_constants[] = {
#ifdef PROC_ASLR_ACTIVE
  ALCOVE_CONSTANT(PROC_ASLR_ACTIVE),
#endif
#ifdef PROC_ASLR_CTL
  ALCOVE_CONSTANT(PROC_ASLR_CTL),
#endif
#ifdef PROC_ASLR_FORCE_DISABLE
  ALCOVE_CONSTANT(PROC_ASLR_FORCE_DISABLE),
#endif
#ifdef PROC_ASLR_FORCE_ENABLE
  ALCOVE_CONSTANT(PROC_ASLR_FORCE_ENABLE),
#endif
#ifdef PROC_ASLR_NOFORCE
  ALCOVE_CONSTANT(PROC_ASLR_NOFORCE),
#endif
#ifdef PROC_ASLR_STATUS
  ALCOVE_CONSTANT(PROC_ASLR_STATUS),
#endif
#ifdef PROC_PDEATHSIG_CTL
  ALCOVE_CONSTANT(PROC_PDEATHSIG_CTL),
#endif
#ifdef PROC_PDEATHSIG_STATUS
  ALCOVE_CONSTANT(PROC_PDEATHSIG_STATUS),
#endif
#ifdef PROC_PROCCTL_MD_MIN
  ALCOVE_CONSTANT(PROC_PROCCTL_MD_MIN),
#endif
#ifdef PROC_PROTMAX_ACTIVE
  ALCOVE_CONSTANT(PROC_PROTMAX_ACTIVE),
#endif
#ifdef PROC_PROTMAX_CTL
  ALCOVE_CONSTANT(PROC_PROTMAX_CTL),
#endif
#ifdef PROC_PROTMAX_FORCE_DISABLE
  ALCOVE_CONSTANT(PROC_PROTMAX_FORCE_DISABLE),
#endif
#ifdef PROC_PROTMAX_FORCE_ENABLE
  ALCOVE_CONSTANT(PROC_PROTMAX_FORCE_ENABLE),
#endif
#ifdef PROC_PROTMAX_NOFORCE
  ALCOVE_CONSTANT(PROC_PROTMAX_NOFORCE),
#endif
#ifdef PROC_PROTMAX_STATUS
  ALCOVE_CONSTANT(PROC_PROTMAX_STATUS),
#endif
#ifdef PROC_REAP_ACQUIRE
  ALCOVE_CONSTANT(PROC_REAP_ACQUIRE),
#endif
#ifdef PROC_REAP_GETPIDS
  ALCOVE_CONSTANT(PROC_REAP_GETPIDS),
#endif
#ifdef PROC_REAP_KILL
  ALCOVE_CONSTANT(PROC_REAP_KILL),
#endif
#ifdef PROC_REAP_RELEASE
  ALCOVE_CONSTANT(PROC_REAP_RELEASE),
#endif
#ifdef PROC_REAP_STATUS
  ALCOVE_CONSTANT(PROC_REAP_STATUS),
#endif
#ifdef PROC_SPROTECT
  ALCOVE_CONSTANT(PROC_SPROTECT),
#endif
#ifdef PROC_STACKGAP_CTL
  ALCOVE_CONSTANT(PROC_STACKGAP_CTL),
#endif
#ifdef PROC_STACKGAP_DISABLE
  ALCOVE_CONSTANT(PROC_STACKGAP_DISABLE),
#endif
#ifdef PROC_STACKGAP_DISABLE_EXEC
  ALCOVE_CONSTANT(PROC_STACKGAP_DISABLE_EXEC),
#endif
#ifdef PROC_STACKGAP_ENABLE
  ALCOVE_CONSTANT(PROC_STACKGAP_ENABLE),
#endif
#ifdef PROC_STACKGAP_ENABLE_EXEC
  ALCOVE_CONSTANT(PROC_STACKGAP_ENABLE_EXEC),
#endif
#ifdef PROC_STACKGAP_STATUS
  ALCOVE_CONSTANT(PROC_STACKGAP_STATUS),
#endif
#ifdef PROC_TRACE_CTL
  ALCOVE_CONSTANT(PROC_TRACE_CTL),
#endif
#ifdef PROC_TRACE_CTL_DISABLE
  ALCOVE_CONSTANT(PROC_TRACE_CTL_DISABLE),
#endif
#ifdef PROC_TRACE_CTL_DISABLE_EXEC
  ALCOVE_CONSTANT(PROC_TRACE_CTL_DISABLE_EXEC),
#endif
#ifdef PROC_TRACE_CTL_ENABLE
  ALCOVE_CONSTANT(PROC_TRACE_CTL_ENABLE),
#endif
#ifdef PROC_TRACE_STATUS
  ALCOVE_CONSTANT(PROC_TRACE_STATUS),
#endif
#ifdef PROC_TRAPCAP_CTL
  ALCOVE_CONSTANT(PROC_TRAPCAP_CTL),
#endif
#ifdef PROC_TRAPCAP_CTL_DISABLE
  ALCOVE_CONSTANT(PROC_TRAPCAP_CTL_DISABLE),
#endif
#ifdef PROC_TRAPCAP_CTL_ENABLE
  ALCOVE_CONSTANT(PROC_TRAPCAP_CTL_ENABLE),
#endif
#ifdef PROC_TRAPCAP_STATUS
  ALCOVE_CONSTANT(PROC_TRAPCAP_STATUS),
#endif
#ifdef REAPER_KILL
  ALCOVE_CONSTANT(REAPER_KILL),
#endif
#ifdef REAPER_KILL_CHILDREN
  ALCOVE_CONSTANT(REAPER_KILL_CHILDREN),
#endif
#ifdef REAPER_KILL_SUBTREE
  ALCOVE_CONSTANT(REAPER_KILL_SUBTREE),
#endif
#ifdef REAPER_PIDINFO_CHILD
  ALCOVE_CONSTANT(REAPER_PIDINFO_CHILD),
#endif
#ifdef REAPER_PIDINFO_REAPER
  ALCOVE_CONSTANT(REAPER_PIDINFO_REAPER),
#endif
#ifdef REAPER_PIDINFO_VALID
  ALCOVE_CONSTANT(REAPER_PIDINFO_VALID),
#endif
#ifdef REAPER_STATUS_OWNED
  ALCOVE_CONSTANT(REAPER_STATUS_OWNED),
#endif
#ifdef REAPER_STATUS_REALINIT
  ALCOVE_CONSTANT(REAPER_STATUS_REALINIT),
#endif

#ifdef P_PID
  ALCOVE_CONSTANT(P_PID),
#endif
#ifdef P_PGID
  ALCOVE_CONSTANT(P_PGID),
#endif
    {NULL, 0}};