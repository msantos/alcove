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

    int
alcove_decode_int(const char *buf, int *index, int *n)
{
    union {
        int i;
        long l;
    } val;

    if (ei_decode_long(buf, index, &val.l) < 0)
        return -1;

    *n = val.i;

    return 0;
}

    int
alcove_decode_uint(const char *buf, int *index, u_int32_t *n)
{
    union {
        u_int32_t i;
        unsigned long l;
    } val;

    if (ei_decode_ulong(buf, index, &val.l) < 0)
        return -1;

    *n = val.i;

    return 0;
}

    ssize_t
alcove_decode_iolist_to_binary(const char *buf, int *index, char *res, size_t *rlen)
{
    int type = 0;
    int arity = 0;

    /* XXX should take an iolist */
    if (ei_get_type(buf, index, &type, &arity) < 0)
        return -1;

    switch (type) {
        case ERL_STRING_EXT:
            if (ei_decode_string(buf, index, res) < 0)
                return -1;

            /* Does not include NULL */
            *rlen = arity;
            break;

        case ERL_BINARY_EXT:
            if (ei_decode_binary(buf, index, res, (long int *)rlen) < 0)
                return -1;
            *rlen = arity;
            break;

        case ERL_NIL_EXT:
            if (ei_decode_list_header(buf, index, &arity) < 0)
                return -1;
            *rlen = arity;
            break;

        case ERL_LIST_EXT: {
            int i = 0;
            size_t offset = 0;
            size_t written = 0;

            if (ei_decode_list_header(buf, index, &arity) < 0)
                return -1;

            /* XXX overflow */
            for (i = 0; i < arity; i++) {
                offset += written;
                if (alcove_decode_iolist_to_binary(buf, index, res+offset, &written) < 0)
                    return -1;
            }

            *rlen = offset + written;
            }
            break;

        default:
            return -1;
    }

    return 0;
}

    char *
alcove_decode_iolist_to_string(const char *buf, int *index)
{
    int type = 0;
    int arity = 0;

    char *res = NULL;
    long rlen = 0;

    /* XXX should take an iolist */
    if (ei_get_type(buf, index, &type, &arity) < 0)
        return NULL;

    res = calloc(arity+1, 1);

    if (!res)
        err(EXIT_FAILURE, "calloc");

    switch (type) {
        case ERL_STRING_EXT:
            if (ei_decode_string(buf, index, res) < 0)
                return NULL;

            break;

        case ERL_BINARY_EXT:
            if (ei_decode_binary(buf, index, res, &rlen) < 0)
                return NULL;

            break;
    }

    return res;
}

    ssize_t
alcove_errno(char *buf, size_t len, int errnum)
{
    return alcove_error(buf, len, erl_errno_id(errnum));
}

    ssize_t
alcove_error(char *buf, size_t len, const char *reason)
{
    int index = 0;
    size_t elen = strlen(reason);

    if (ei_encode_version(buf, &index) < 0)
        return -1;

    if (ei_encode_tuple_header(buf, &index, 2) < 0)
        return -1;

    if (ei_encode_atom(buf, &index, "error") < 0)
        return -1;

    if (elen > len - index)
        return -1;

    if (ei_encode_atom_len(buf, &index, reason, strlen(reason)) < 0)
        return -1;

    return index;
}

    ssize_t
alcove_mk_atom(char *buf, size_t len, const char *atom)
{
    int index = 0;
    size_t slen = strlen(atom);

    if (ei_encode_version(buf, &index) < 0)
        return -1;

    if (slen > len - index)
        return -1;

    if (ei_encode_atom_len(buf, &index, atom, strlen(atom)) < 0)
        return -1;

    return index;
}

    ssize_t
alcove_mk_binary(char *buf, size_t buflen, void *bin, size_t len)
{
    int index = 0;

    if (ei_encode_version(buf, &index) < 0)
        return -1;

    if (len > buflen - index)
        return -1;

    if (ei_encode_binary(buf, &index, bin, len) < 0)
        return -1;

    return index;
}

    ssize_t
alcove_mk_long(char *buf, size_t buflen, long n)
{
    int index = 0;

    if (ei_encode_version(buf, &index) < 0)
        return -1;

    if (ei_encode_long(buf, &index, n) < 0)
        return -1;

    return index;
}

    ssize_t
alcove_mk_ulong(char *buf, size_t buflen, unsigned long n)
{
    int index = 0;

    if (ei_encode_version(buf, &index) < 0)
        return -1;

    if (ei_encode_ulong(buf, &index, n) < 0)
        return -1;

    return index;
}

    void *
alcove_malloc(ssize_t size)
{
    void *buf = NULL;

    if (size <= 0 || size >= INT32_MAX)
        errx(EXIT_FAILURE, "malloc:invalid size:%ld",
                (unsigned long)size);

    buf = malloc(size);

    if (!buf)
        err(EXIT_FAILURE, "malloc");

    return buf;
}

    int
alcove_define(char *buf, int *index, char *name, alcove_define_t *constants)
{
    alcove_define_t *dp = NULL;

    for (dp = constants; dp->name != NULL; dp++) {
        if (!strcmp(name, dp->name))
            return ei_encode_ulonglong(buf, index, dp->val);
    }

    return ei_encode_atom(buf, index, "false");
}

    int
alcove_constant(char *buf, int *index, u_int64_t val,
        alcove_define_t *constants)
{
    alcove_define_t *dp = NULL;

    for (dp = constants; dp->name != NULL; dp++) {
        if (val == dp->val)
            return ei_encode_atom(buf, index, dp->name);
    }

    return ei_encode_atom(buf, index, "false");
}

    int
alcove_str_to_argv(const char *arg, int *index, int arity, char *buf, size_t *len)
{
    int rindex = 0;

    char tmp[MAXMSGLEN] = {0};
    int i = 0;

    /* [0] is also encoded to a string. strlen() can't be used to
       calculate the length */
    if (ei_decode_string(arg, index, tmp) < 0)
        return -1;

    /* Must be small integers from 0 - 255 */
    for (i = 0; i < arity; i++) {
        if (ei_encode_ulong(buf, &rindex, tmp[i]) < 0)
            return -1;
    }

    if (ei_encode_empty_list(buf, &rindex) < 0)
        return -1;

    *len = rindex;

    return 0;
}

    char **
alcove_list_to_argv(const char *arg, int *index)
{
    int arity = 0;

    int i = 0;
    char **argv = NULL;
    long maxarg = sysconf(_SC_ARG_MAX);

    if (ei_decode_list_header(arg, index, &arity) < 0)
        return NULL;

    if (arity < 0 || arity >= maxarg)
        return NULL;

    /* NULL terminate */
    argv = calloc(arity + 1, sizeof(char *));

    if (!argv)
        err(EXIT_FAILURE, "calloc");

    for (i = 0; i < arity; i++) {
        argv[i] = alcove_decode_iolist_to_string(arg, index);
        if (!argv[i])
            goto ERR;
    }

    /* list tail */
    if (ei_decode_list_header(arg, index, &arity) < 0 || arity != 0)
        goto ERR;

    return argv;

ERR:
    alcove_free_argv(argv);
    return NULL;
}

    void
alcove_free_argv(char **argv)
{
    int i = 0;

    if (argv == NULL)
        return;

    for (i = 0; argv[i]; i++)
        free(argv[i]);

    free(argv);
}

/* XXX FIXME */
    void *
alcove_list_to_buf(const char *arg, int *index, size_t *len,
        alcove_alloc_t **ptr, ssize_t *nptr)
{
    int type = 0;
    int arity = 0;
    long size = 0;

    const char *parg = NULL;
    int pindex = 0;
    int parity = 0;

    char *buf = NULL;
    char *pbuf = NULL;
    char tmp[MAXMSGLEN] = {0};
    unsigned long val = 0;

    int i = 0;
    size_t n = 0;

    *len = 0;
    *nptr = 0;

    if (ei_decode_list_header(arg, index, &arity) < 0)
        return NULL;

    if (arity < 0 || arity >= 0xffff)
        return NULL;

    parg = arg;
    pindex = *index;
    parity = arity;

    /* Calculate the size required */
    for (i = 0; i < arity; i++) {
        if (ei_get_type(parg, &pindex, &type, &parity) < 0)
            return NULL;

        switch (type) {
            case ERL_BINARY_EXT:
                if (parity > sizeof(tmp))
                    return NULL;

                if (ei_decode_binary(parg, &pindex, tmp, &size) < 0)
                    return NULL;

                n += size;
                break;

            case ERL_SMALL_TUPLE_EXT:
            case ERL_LARGE_TUPLE_EXT:
                if (parity != 2)
                    return NULL;

                if (ei_decode_tuple_header(parg, &pindex, &parity) < 0)
                    return NULL;

                if (ei_decode_atom(parg, &pindex, tmp) < 0)
                    return NULL;

                if (strncmp(tmp, "ptr", 3))
                    return NULL;

                if (ei_get_type(parg, &pindex, &type, &parity) < 0)
                    return NULL;

                switch (type) {
                    case ERL_SMALL_INTEGER_EXT:
                    case ERL_INTEGER_EXT:
                        if (ei_decode_ulong(parg, &pindex, &val) < 0)
                            return NULL;

                        n += sizeof(void *);
                        break;

                    case ERL_BINARY_EXT:
                        if (ei_decode_binary(parg, &pindex,
                                    tmp, &size) < 0)
                            return NULL;

                        n += sizeof(void *);
                        break;
                }
                break;

            default:
                return NULL;
        }
    }

    buf = alcove_malloc(n);
    *len = n;

    *ptr = alcove_malloc(arity * sizeof(alcove_alloc_t));
    *nptr = arity;

    pbuf = buf;

    /* Copy the list contents */
    for (i = 0; i < arity; i++) {
        (void)ei_get_type(arg, index, &type, &parity);

        switch (type) {
            case ERL_BINARY_EXT:
                (void)ei_decode_binary(arg, index, buf, &size);
                buf += size;
                (*ptr)[i].p = NULL;
                (*ptr)[i].len = size;
                break;

            case ERL_SMALL_TUPLE_EXT:
            case ERL_LARGE_TUPLE_EXT:
                (void)ei_decode_tuple_header(arg, index, &parity);
                (void)ei_decode_atom(arg, index, tmp);
                (void)ei_get_type(arg, index, &type, &parity);

                switch (type) {
                    case ERL_SMALL_INTEGER_EXT:
                    case ERL_INTEGER_EXT: {
                        char *p = NULL;

                        (void)ei_decode_ulong(arg, index, &val);

                        p = calloc(val, 1);
                        if (!p)
                            err(EXIT_FAILURE, "calloc");

                        (void)memcpy(buf, &p, sizeof(void *));
                        buf += sizeof(void *);
                        (*ptr)[i].p = p;
                        (*ptr)[i].len = val;
                        }
                        break;

                    case ERL_BINARY_EXT: {
                        char *p = NULL;
                        (void)ei_decode_binary(arg, index, tmp, &size);
                        p = alcove_malloc(size);
                        (void)memcpy(p, tmp, size);
                        (void)memcpy(buf, &p, sizeof(void *));
                        buf += sizeof(void *);
                        (*ptr)[i].p = p;
                        (*ptr)[i].len = size;
                        }
                        break;
                }
        }
    }

    return pbuf;
}

    int
alcove_buf_to_list(char *reply, int *rindex, const char *buf, size_t len,
        alcove_alloc_t *ptr, ssize_t nptr)
{
    int i = 0;
    size_t offset = 0;

    if (ei_encode_list_header(reply, rindex, nptr) < 0)
        return -1;

    for ( ; i < nptr; i++) {
        if (ptr[i].p) {
            /* Allocated buffer */
            if (ei_encode_tuple_header(reply, rindex, 2) < 0)
                return -1;
            if (ei_encode_atom(reply, rindex, "ptr") < 0)
                return -1;
            if (ei_encode_binary(reply, rindex, ptr[i].p,
                        ptr[i].len) < 0)
                return -1;
            free(ptr[i].p);
            offset += sizeof(void *);
        }
        else {
            /* Static binary */
            if (ei_encode_binary(reply, rindex, buf+offset, ptr[i].len) < 0)
                return -1;
            offset += ptr[i].len;
        }
    }

    if (ei_encode_empty_list(reply, rindex) < 0)
        return -1;

    return 0;
}
