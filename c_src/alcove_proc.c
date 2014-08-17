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
#ifdef HAVE_SECCOMP
#include <linux/seccomp.h>
#endif
#include "alcove_proc.h"
#endif

/*
 * getpid(2)
 *
 */
    ssize_t
alcove_getpid(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    return alcove_mk_long(reply, rlen, getpid());
}

/*
 * prctl(2)
 *
 */
    ssize_t
alcove_prctl(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
#ifdef __linux__
    int index = 0;
    int rindex = 0;
    int type = 0;
    int arity = 0;

    int option = 0;
    alcove_alloc_t *elem[4] = {0};
    ssize_t nelem[4] = {0};
    int i = 0;

    alcove_prctl_arg_t prarg[4] = {{0}};

    int rv = 0;

    /* option */
    if (alcove_decode_int(arg, &index, &option) < 0)
        return -1;

    /* arg2, arg3, arg4, arg5 */
    for (i = 0; i < 4; i++) {
        if (ei_get_type(arg, &index, &type, &arity) < 0)
            return -1;

        switch (type) {
            case ERL_SMALL_INTEGER_EXT:
            case ERL_INTEGER_EXT:
                if (ei_decode_ulong(arg, &index, &prarg[i].arg) < 0)
                    return -1;

                break;

            case ERL_LIST_EXT:
                prarg[i].type = ALCOVE_PRARG_CSTRUCT;
                prarg[i].data = alcove_list_to_buf(arg, &index, &(prarg[i].len),
                        &(elem[i]), &(nelem[i]));
                break;

            case ERL_BINARY_EXT:
                prarg[i].type = ALCOVE_PRARG_BINARY;
                prarg[i].data = alcove_malloc(arity);
                if (ei_decode_binary(arg, &index, prarg[i].data,
                            (long int *)&(prarg[i].len)) < 0)
                    return -1;

                break;

            case ERL_NIL_EXT:
                if (ei_decode_list_header(arg, &index, &arity) < 0 ||
                        arity != 0)
                    return -1;

                break;

            default:
                return -1;
        }
    }

    rv = prctl(option, PRARG(prarg[0]), PRARG(prarg[1]),
            PRARG(prarg[2]), PRARG(prarg[3]));

    if (rv < 0)
        return alcove_errno(reply, rlen, errno);

    ALCOVE_ERR(alcove_encode_version(reply, rlen, &rindex));
    ALCOVE_ERR(alcove_encode_tuple_header(reply, rlen, &rindex, 6));
    ALCOVE_ERR(alcove_encode_atom(reply, rlen, &rindex, "ok"));
    ALCOVE_ERR(alcove_encode_long(reply, rlen, &rindex, rv));

    for (i = 0; i < 4; i++) {
        switch (prarg[i].type) {
            case ALCOVE_PRARG_UNSIGNED_LONG:
                ALCOVE_ERR(alcove_encode_ulonglong(reply, rlen, &rindex, prarg[i].arg));
                break;
            case ALCOVE_PRARG_CSTRUCT:
                ALCOVE_ERR(alcove_buf_to_list(reply, &rindex,
                            prarg[i].data, prarg[i].len,
                            elem[i], nelem[i]));
                free(prarg[i].data);
                break;
            case ALCOVE_PRARG_BINARY:
                ALCOVE_ERR(alcove_encode_binary(reply, rlen, &rindex, prarg[i].data,
                            prarg[i].len));
                free(prarg[i].data);
                break;
            default:
                return -1;
        }
    }

    return rindex;
#else
    return alcove_error(reply, rlen, "unsupported");
#endif
}

/*
 * prctl flags
 *
 */
    ssize_t
alcove_prctl_define(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
#ifdef __linux__
    int index = 0;
    int rindex = 0;

    char name[MAXATOMLEN] = {0};

    /* name */
    if (ei_decode_atom(arg, &index, name) < 0)
        return -1;

    ALCOVE_ERR(alcove_encode_version(reply, rlen, &rindex));
    ALCOVE_ERR(alcove_define(reply, &rindex, name, alcove_prctl_constants));
    return rindex;
#else
    return alcove_mk_atom(reply, rlen, "false");
#endif
}

/*
 * getsid(2)
 */
    ssize_t
alcove_getsid(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;
    int rindex = 0;

    pid_t pid = 0;
    pid_t rv = 0;

    /* pid */
    if (alcove_decode_int(arg, &index, &pid) < 0)
        return -1;

    rv = getsid(pid);

    if (rv < 0)
        return alcove_errno(reply, rlen, errno);

    ALCOVE_OK(reply, &rindex,
        alcove_encode_long(reply, rlen, &rindex, rv));

    return rindex;
}

/*
 * setsid(2)
 */
    ssize_t
alcove_setsid(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int rindex = 0;
    pid_t pid = setsid();

    if (pid < 0)
        return alcove_errno(reply, rlen, errno);

    ALCOVE_OK(reply, &rindex,
        alcove_encode_long(reply, rlen, &rindex, pid));

    return rindex;
}

/*
 * getpgrp(2)
 */
    ssize_t
alcove_getpgrp(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    return alcove_mk_long(reply, rlen, getpgrp());
}
