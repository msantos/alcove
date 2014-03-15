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

#ifdef __linux__
#include <sys/prctl.h>

typedef struct {
    u_char type;
    unsigned long arg;
    char *data;
    size_t len;
} alcove_prctl_arg_t;

#define PRARG(x) ((x.type) ? (unsigned long)x.data : x.arg)
#define PRVAL(x) ((x.type) ? erl_mk_binary(x.data, x.len) : erl_mk_ulonglong(x.arg))

#define PROPT(_term, _arg) do { \
    if (ERL_IS_INTEGER(_term)) { \
        _arg.arg = ERL_INT_UVALUE(_term); \
    } \
    else if (ALCOVE_IS_IOLIST(_term)) { \
        ETERM *bin = NULL; \
        bin = erl_iolist_to_binary(_term); \
        _arg.type = 1; \
        _arg.data = alcove_malloc(_arg.len); \
        (void)memcpy(_arg.data, ERL_BIN_PTR(bin), ERL_BIN_SIZE(bin)); \
    } \
} while (0)
#endif

/*
 * getpid(2)
 *
 */
    ETERM *
alcove_getpid(alcove_state_t *ap, ETERM *arg)
{
    return erl_mk_int(getpid());
}

/*
 * prctl(2)
 *
 */
    ETERM *
alcove_prctl(alcove_state_t *ap, ETERM *arg)
{
#ifdef __linux__
    ETERM *hd = NULL;
    int option = 0;

    alcove_prctl_arg_t arg2 = {0};
    alcove_prctl_arg_t arg3 = {0};
    alcove_prctl_arg_t arg4 = {0};
    alcove_prctl_arg_t arg5 = {0};

    int rv = 0;
    ETERM *t[6] = {0};

    /* option */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_INTEGER(hd))
        goto BADARG;

    option = ERL_INT_VALUE(hd);

    /* XXX unsigned long vs unsigned int */

    /* arg2 */
    arg = alcove_list_head(&hd, arg);
    if (!hd)
        goto BADARG;

    PROPT(hd, arg2);

    /* arg3 */
    arg = alcove_list_head(&hd, arg);
    if (!hd)
        goto BADARG;

    PROPT(hd, arg3);

    /* arg4 */
    arg = alcove_list_head(&hd, arg);
    if (!hd)
        goto BADARG;

    PROPT(hd, arg3);

    /* arg5 */
    arg = alcove_list_head(&hd, arg);
    if (!hd)
        goto BADARG;

    PROPT(hd, arg3);

    rv = prctl(option, PRARG(arg2), PRARG(arg3), PRARG(arg4), PRARG(arg5));

    /* XXX On  success,  PR_GET_DUMPABLE,  PR_GET_KEEPCAPS,  PR_CAPBSET_READ,
     * XXX PR_GET_TIMING,  PR_GET_SECUREBITS, PR_MCE_KILL_GET, and (if it
     * XXX returns) PR_GET_SECCOMP return the nonnegative values described
     * XXX above.  All other option values return 0 on success.  On error,
     * XXX -1 is returned,  and errno is set appropriately.
     */
    if (rv < 0)
        return alcove_errno(errno);

    t[0] = erl_mk_atom("ok");
    t[1] = erl_mk_int(rv);
    t[2] = PRVAL(arg2);
    t[3] = PRVAL(arg3);
    t[4] = PRVAL(arg4);
    t[5] = PRVAL(arg5);

    return erl_mk_tuple(t, 6);

BADARG:
    return erl_mk_atom("badarg");
#else
    return alcove_error("unsupported");
#endif
}

/*
 * prctl flags
 *
 */
    ETERM *
alcove_prctl_define(alcove_state_t *ap, ETERM *arg)
{
#ifdef __linux__
    ETERM *hd = NULL;
    char *flag = NULL;

    /* flag */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_ATOM(hd))
        goto BADARG;

    flag = ERL_ATOM_PTR(hd);

    if (!strncmp(flag, "set_pdeathsig", 13)) return erl_mk_ulonglong(PR_SET_PDEATHSIG);
#ifdef PR_GET_PDEATHSIG
    else if (!strncmp(flag, "get_pdeathsig", 13)) return erl_mk_ulonglong(PR_GET_PDEATHSIG);
#endif
#ifdef PR_GET_DUMPABLE
    else if (!strncmp(flag, "get_dumpable", 12)) return erl_mk_ulonglong(PR_GET_DUMPABLE);
#endif
#ifdef PR_SET_DUMPABLE
    else if (!strncmp(flag, "set_dumpable", 12)) return erl_mk_ulonglong(PR_SET_DUMPABLE);
#endif
#ifdef PR_GET_UNALIGN
    else if (!strncmp(flag, "get_unalign", 11)) return erl_mk_ulonglong(PR_GET_UNALIGN);
#endif
#ifdef PR_SET_UNALIGN
    else if (!strncmp(flag, "set_unalign", 11)) return erl_mk_ulonglong(PR_SET_UNALIGN);
#endif
#ifdef PR_UNALIGN_NOPRINT
    else if (!strncmp(flag, "unalign_noprint", 15)) return erl_mk_ulonglong(PR_UNALIGN_NOPRINT);
#endif
#ifdef PR_UNALIGN_SIGBUS
    else if (!strncmp(flag, "unalign_sigbus", 14)) return erl_mk_ulonglong(PR_UNALIGN_SIGBUS);
#endif
#ifdef PR_GET_KEEPCAPS
    else if (!strncmp(flag, "get_keepcaps", 12)) return erl_mk_ulonglong(PR_GET_KEEPCAPS);
#endif
#ifdef PR_SET_KEEPCAPS
    else if (!strncmp(flag, "set_keepcaps", 12)) return erl_mk_ulonglong(PR_SET_KEEPCAPS);
#endif
#ifdef PR_GET_FPEMU
    else if (!strncmp(flag, "get_fpemu", 9)) return erl_mk_ulonglong(PR_GET_FPEMU);
#endif
#ifdef PR_SET_FPEMU
    else if (!strncmp(flag, "set_fpemu", 9)) return erl_mk_ulonglong(PR_SET_FPEMU);
#endif
#ifdef PR_FPEMU_NOPRINT
    else if (!strncmp(flag, "fpemu_noprint", 13)) return erl_mk_ulonglong(PR_FPEMU_NOPRINT);
#endif
#ifdef PR_FPEMU_SIGFPE
    else if (!strncmp(flag, "fpemu_sigfpe", 12)) return erl_mk_ulonglong(PR_FPEMU_SIGFPE);
#endif
#ifdef PR_GET_FPEXC
    else if (!strncmp(flag, "get_fpexc", 9)) return erl_mk_ulonglong(PR_GET_FPEXC);
#endif
#ifdef PR_SET_FPEXC
    else if (!strncmp(flag, "set_fpexc", 9)) return erl_mk_ulonglong(PR_SET_FPEXC);
#endif
#ifdef PR_FP_EXC_SW_ENABLE
    else if (!strncmp(flag, "fp_exc_sw_enable", 16)) return erl_mk_ulonglong(PR_FP_EXC_SW_ENABLE);
#endif
#ifdef PR_FP_EXC_DIV
    else if (!strncmp(flag, "fp_exc_div", 10)) return erl_mk_ulonglong(PR_FP_EXC_DIV);
#endif
#ifdef PR_FP_EXC_OVF
    else if (!strncmp(flag, "fp_exc_ovf", 10)) return erl_mk_ulonglong(PR_FP_EXC_OVF);
#endif
#ifdef PR_FP_EXC_UND
    else if (!strncmp(flag, "fp_exc_und", 10)) return erl_mk_ulonglong(PR_FP_EXC_UND);
#endif
#ifdef PR_FP_EXC_RES
    else if (!strncmp(flag, "fp_exc_res", 10)) return erl_mk_ulonglong(PR_FP_EXC_RES);
#endif
#ifdef PR_FP_EXC_INV
    else if (!strncmp(flag, "fp_exc_inv", 10)) return erl_mk_ulonglong(PR_FP_EXC_INV);
#endif
#ifdef PR_FP_EXC_DISABLED
    else if (!strncmp(flag, "fp_exc_disabled", 15)) return erl_mk_ulonglong(PR_FP_EXC_DISABLED);
#endif
#ifdef PR_FP_EXC_NONRECOV
    else if (!strncmp(flag, "fp_exc_nonrecov", 15)) return erl_mk_ulonglong(PR_FP_EXC_NONRECOV);
#endif
#ifdef PR_FP_EXC_ASYNC
    else if (!strncmp(flag, "fp_exc_async", 12)) return erl_mk_ulonglong(PR_FP_EXC_ASYNC);
#endif
#ifdef PR_FP_EXC_PRECISE
    else if (!strncmp(flag, "fp_exc_precise", 14)) return erl_mk_ulonglong(PR_FP_EXC_PRECISE);
#endif
#ifdef PR_GET_TIMING
    else if (!strncmp(flag, "get_timing", 10)) return erl_mk_ulonglong(PR_GET_TIMING);
#endif
#ifdef PR_SET_TIMING
    else if (!strncmp(flag, "set_timing", 10)) return erl_mk_ulonglong(PR_SET_TIMING);
#endif
#ifdef PR_TIMING_STATISTICAL
    else if (!strncmp(flag, "timing_statistical", 18)) return erl_mk_ulonglong(PR_TIMING_STATISTICAL);
#endif
#ifdef PR_TIMING_TIMESTAMP
    else if (!strncmp(flag, "timing_timestamp", 16)) return erl_mk_ulonglong(PR_TIMING_TIMESTAMP);
#endif
#ifdef PR_SET_NAME
    else if (!strncmp(flag, "set_name", 8)) return erl_mk_ulonglong(PR_SET_NAME);
#endif
#ifdef PR_GET_NAME
    else if (!strncmp(flag, "get_name", 8)) return erl_mk_ulonglong(PR_GET_NAME);
#endif
#ifdef PR_GET_ENDIAN
    else if (!strncmp(flag, "get_endian", 10)) return erl_mk_ulonglong(PR_GET_ENDIAN);
#endif
#ifdef PR_SET_ENDIAN
    else if (!strncmp(flag, "set_endian", 10)) return erl_mk_ulonglong(PR_SET_ENDIAN);
#endif
#ifdef PR_ENDIAN_BIG
    else if (!strncmp(flag, "endian_big", 10)) return erl_mk_ulonglong(PR_ENDIAN_BIG);
#endif
#ifdef PR_ENDIAN_LITTLE
    else if (!strncmp(flag, "endian_little", 13)) return erl_mk_ulonglong(PR_ENDIAN_LITTLE);
#endif
#ifdef PR_ENDIAN_PPC_LITTLE
    else if (!strncmp(flag, "endian_ppc_little", 17)) return erl_mk_ulonglong(PR_ENDIAN_PPC_LITTLE);
#endif
#ifdef PR_GET_SECCOMP
    else if (!strncmp(flag, "get_seccomp", 11)) return erl_mk_ulonglong(PR_GET_SECCOMP);
#endif
#ifdef PR_SET_SECCOMP
    else if (!strncmp(flag, "set_seccomp", 11)) return erl_mk_ulonglong(PR_SET_SECCOMP);
#endif
#ifdef PR_CAPBSET_READ
    else if (!strncmp(flag, "capbset_read", 12)) return erl_mk_ulonglong(PR_CAPBSET_READ);
#endif
#ifdef PR_CAPBSET_DROP
    else if (!strncmp(flag, "capbset_drop", 12)) return erl_mk_ulonglong(PR_CAPBSET_DROP);
#endif
#ifdef PR_GET_TSC
    else if (!strncmp(flag, "get_tsc", 7)) return erl_mk_ulonglong(PR_GET_TSC);
#endif
#ifdef PR_SET_TSC
    else if (!strncmp(flag, "set_tsc", 7)) return erl_mk_ulonglong(PR_SET_TSC);
#endif
#ifdef PR_TSC_ENABLE
    else if (!strncmp(flag, "tsc_enable", 10)) return erl_mk_ulonglong(PR_TSC_ENABLE);
#endif
#ifdef PR_TSC_SIGSEGV
    else if (!strncmp(flag, "tsc_sigsegv", 11)) return erl_mk_ulonglong(PR_TSC_SIGSEGV);
#endif
#ifdef PR_GET_SECUREBITS
    else if (!strncmp(flag, "get_securebits", 14)) return erl_mk_ulonglong(PR_GET_SECUREBITS);
#endif
#ifdef PR_SET_SECUREBITS
    else if (!strncmp(flag, "set_securebits", 14)) return erl_mk_ulonglong(PR_SET_SECUREBITS);
#endif
#ifdef PR_SET_TIMERSLACK
    else if (!strncmp(flag, "set_timerslack", 14)) return erl_mk_ulonglong(PR_SET_TIMERSLACK);
#endif
#ifdef PR_GET_TIMERSLACK
    else if (!strncmp(flag, "get_timerslack", 14)) return erl_mk_ulonglong(PR_GET_TIMERSLACK);
#endif
#ifdef PR_TASK_PERF_EVENTS_DISABLE
    else if (!strncmp(flag, "task_perf_events_disable", 24)) return erl_mk_ulonglong(PR_TASK_PERF_EVENTS_DISABLE);
#endif
#ifdef PR_TASK_PERF_EVENTS_ENABLE
    else if (!strncmp(flag, "task_perf_events_enable", 23)) return erl_mk_ulonglong(PR_TASK_PERF_EVENTS_ENABLE);
#endif
#ifdef PR_MCE_KILL
    else if (!strncmp(flag, "mce_kill", 8)) return erl_mk_ulonglong(PR_MCE_KILL);
#endif
#ifdef PR_MCE_KILL_CLEAR
    else if (!strncmp(flag, "mce_kill_clear", 14)) return erl_mk_ulonglong(PR_MCE_KILL_CLEAR);
#endif
#ifdef PR_MCE_KILL_SET
    else if (!strncmp(flag, "mce_kill_set", 12)) return erl_mk_ulonglong(PR_MCE_KILL_SET);
#endif
#ifdef PR_MCE_KILL_LATE
    else if (!strncmp(flag, "mce_kill_late", 13)) return erl_mk_ulonglong(PR_MCE_KILL_LATE);
#endif
#ifdef PR_MCE_KILL_EARLY
    else if (!strncmp(flag, "mce_kill_early", 14)) return erl_mk_ulonglong(PR_MCE_KILL_EARLY);
#endif
#ifdef PR_MCE_KILL_DEFAULT
    else if (!strncmp(flag, "mce_kill_default", 16)) return erl_mk_ulonglong(PR_MCE_KILL_DEFAULT);
#endif
#ifdef PR_MCE_KILL_GET
    else if (!strncmp(flag, "mce_kill_get", 12)) return erl_mk_ulonglong(PR_MCE_KILL_GET);
#endif
#ifdef PR_SET_MM
    else if (!strncmp(flag, "set_mm", 6)) return erl_mk_ulonglong(PR_SET_MM);
#endif
#ifdef PR_SET_MM_START_CODE
    else if (!strncmp(flag, "set_mm_start_code", 17)) return erl_mk_ulonglong(PR_SET_MM_START_CODE);
#endif
#ifdef PR_SET_MM_END_CODE
    else if (!strncmp(flag, "set_mm_end_code", 15)) return erl_mk_ulonglong(PR_SET_MM_END_CODE);
#endif
#ifdef PR_SET_MM_START_DATA
    else if (!strncmp(flag, "set_mm_start_data", 17)) return erl_mk_ulonglong(PR_SET_MM_START_DATA);
#endif
#ifdef PR_SET_MM_END_DATA
    else if (!strncmp(flag, "set_mm_end_data", 15)) return erl_mk_ulonglong(PR_SET_MM_END_DATA);
#endif
#ifdef PR_SET_MM_START_STACK
    else if (!strncmp(flag, "set_mm_start_stack", 18)) return erl_mk_ulonglong(PR_SET_MM_START_STACK);
#endif
#ifdef PR_SET_MM_START_BRK
    else if (!strncmp(flag, "set_mm_start_brk", 16)) return erl_mk_ulonglong(PR_SET_MM_START_BRK);
#endif
#ifdef PR_SET_MM_BRK
    else if (!strncmp(flag, "set_mm_brk", 10)) return erl_mk_ulonglong(PR_SET_MM_BRK);
#endif
#ifdef PR_SET_PTRACER
    else if (!strncmp(flag, "set_ptracer", 11)) return erl_mk_ulonglong(PR_SET_PTRACER);
#endif
#ifdef PR_SET_PTRACER_ANY
    else if (!strncmp(flag, "set_ptracer_any", 15)) return erl_mk_ulonglong(PR_SET_PTRACER_ANY);
#endif
#ifdef PR_SET_NO_NEW_PRIVS
    else if (!strncmp(flag, "set_no_new_privs", 16)) return erl_mk_ulonglong(PR_SET_NO_NEW_PRIVS);
#endif
#ifdef PR_GET_NO_NEW_PRIVS
    else if (!strncmp(flag, "get_no_new_privs", 16)) return erl_mk_ulonglong(PR_GET_NO_NEW_PRIVS);
#endif
    else return erl_mk_atom("false");

BADARG:
    return erl_mk_atom("badarg");
#else
    return erl_mk_atom("false");
#endif
}
