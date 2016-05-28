/* Copyright (c) 2016, Michael Santos <michael.santos@gmail.com>
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

#if defined(__linux__)
#include <sys/ptrace.h>
#include <linux/ptrace.h>
#include "alcove_ptrace_constants.h"

typedef struct {
    u_char type;
    unsigned long arg;
    char data[MAXMSGLEN];
    size_t len;
} alcove_ptrace_arg_t;

enum {
    ALCOVE_PTRACEARG_INT,
    ALCOVE_PTRACEARG_CSTRUCT,
    ALCOVE_PTRACEARG_BINARY
};
#endif

#define PRTRACEARG(x) (((x).type) ? (x).data : (void *)(x).arg)

/*
 * ptrace(2)
 *
 */
    ssize_t
alcove_sys_ptrace(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
#if defined(__linux__)
    int index = 0;
    int rindex = 0;
    int type = 0;
    int arity = 0;

    int request = 0;
    pid_t pid = 0;
    alcove_ptrace_arg_t addr;
    alcove_ptrace_arg_t data;
    alcove_alloc_t *elem = NULL;
    ssize_t nelem = 0;

    long rv = 0;

    UNUSED(ap);

    /* -Wmissing-field-initializers */
    (void)memset(&addr, 0, sizeof(addr));
    (void)memset(&data, 0, sizeof(data));

    /* request */
    switch (alcove_decode_constant(arg, len, &index, (int *)&request,
                alcove_ptrace_constants)) {
        case 0:
            break;
        case 1:
            return alcove_mk_error(reply, rlen, "enotsup");
        default:
            return -1;
    }

    /* pid */
    if (alcove_decode_int(arg, len, &index, &pid) < 0)
        return -1;

    /* addr */
    if (alcove_get_type(arg, len, &index, &type, &arity) < 0)
        return -1;

    switch (type) {
        case ERL_SMALL_INTEGER_EXT:
        case ERL_INTEGER_EXT:
            addr.type = ALCOVE_PTRACEARG_INT;
            if (alcove_decode_ulong(arg, len, &index, &addr.arg) < 0)
                return -1;

            break;

        case ERL_LIST_EXT:
            addr.type = ALCOVE_PTRACEARG_CSTRUCT;
            addr.len = sizeof(addr.data);
            if (alcove_decode_cstruct(arg, len, &index, addr.data,
                &(addr.len), &elem, &nelem) < 0)
                return -1;

            break;

        case ERL_BINARY_EXT:
            addr.type = ALCOVE_PTRACEARG_BINARY;
            if (arity > sizeof(addr.data))
                return -1;
            if (alcove_decode_binary(arg, len, &index,
                        addr.data, &addr.len) < 0)
                return -1;

            break;

        default:
            return -1;
    }

    /* data */
    if (alcove_get_type(arg, len, &index, &type, &arity) < 0)
        return -1;

    switch (type) {
        case ERL_SMALL_INTEGER_EXT:
        case ERL_INTEGER_EXT:
            data.type = ALCOVE_PTRACEARG_INT;
            if (alcove_decode_ulong(arg, len, &index, &data.arg) < 0)
                return -1;

            break;

        case ERL_LIST_EXT:
            data.type = ALCOVE_PTRACEARG_CSTRUCT;
            data.len = sizeof(data.data);
            if (alcove_decode_cstruct(arg, len, &index, data.data,
                &(data.len), &elem, &nelem) < 0)
                return -1;

            break;

        case ERL_BINARY_EXT:
            data.type = ALCOVE_PTRACEARG_BINARY;
            if (arity > sizeof(data.data))
                return -1;
            if (alcove_decode_binary(arg, len, &index,
                        data.data, &data.len) < 0)
                return -1;

            break;

        default:
            return -1;
    }

    errno = 0;
    rv = ptrace(request, pid, PRTRACEARG(addr), PRTRACEARG(data));

    if (errno)
        return alcove_mk_errno(reply, rlen, errno);

    ALCOVE_ERR(alcove_encode_version(reply, rlen, &rindex));
    ALCOVE_ERR(alcove_encode_tuple_header(reply, rlen, &rindex, 4));
    ALCOVE_ERR(alcove_encode_atom(reply, rlen, &rindex, "ok"));
    ALCOVE_ERR(alcove_encode_long(reply, rlen, &rindex, rv));

    switch (addr.type) {
        case ALCOVE_PTRACEARG_CSTRUCT:
            ALCOVE_ERR(alcove_encode_cstruct(reply, rlen, &rindex,
                        addr.data, addr.len, elem, nelem));
            break;
        case ALCOVE_PTRACEARG_INT: /* return an empty binary */
        case ALCOVE_PTRACEARG_BINARY:
            ALCOVE_ERR(alcove_encode_binary(reply, rlen, &rindex, addr.data,
                        addr.len));
            break;
        default:
            return -1;
    }

    switch (data.type) {
        case ALCOVE_PTRACEARG_CSTRUCT:
            ALCOVE_ERR(alcove_encode_cstruct(reply, rlen, &rindex,
                        data.data, data.len, elem, nelem));
            break;
        case ALCOVE_PTRACEARG_INT: /* return an empty binary */
        case ALCOVE_PTRACEARG_BINARY:
            ALCOVE_ERR(alcove_encode_binary(reply, rlen, &rindex, data.data,
                        data.len));
            break;
        default:
            return -1;
    }

    return rindex;
#else
    UNUSED(ap);
    UNUSED(arg);
    UNUSED(len);

    return alcove_mk_atom(reply, rlen, "undef");
#endif
}
