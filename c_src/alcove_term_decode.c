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

static void *alcove_malloc(ssize_t);
static char *alcove_x_decode_iolist_to_string(const char *buf, size_t len,
        int *index);
static int alcove_decode_iolist_internal(const char *buf, size_t len,
        int *index, char *res, size_t rlen, int *rindex, int depth);

    int
alcove_decode_int(const char *buf, size_t len, int *index, int *n)
{
    union {
        int i;
        long l;
    } val;

    if (alcove_decode_long(buf, len, index, &val.l) < 0)
        return -1;

    *n = val.i;

    return 0;
}

    int
alcove_decode_uint(const char *buf, size_t len, int *index, u_int32_t *n)
{
    union {
        u_int32_t i;
        unsigned long l;
    } val;

    if (alcove_decode_ulong(buf, len, index, &val.l) < 0)
        return -1;

    *n = val.i;

    return 0;
}

    int
alcove_decode_long(const char *buf, size_t len, int *index, long *p)
{
    int type = 0;
    int arity = 0;

    if (alcove_get_type(buf, len, index, &type, &arity) < 0)
        return -1;

    return ei_decode_long(buf, index, p);
}

    int
alcove_decode_ulong(const char *buf, size_t len, int *index, unsigned long *p)
{
    int type = 0;
    int arity = 0;

    if (alcove_get_type(buf, len, index, &type, &arity) < 0)
        return -1;

    return ei_decode_ulong(buf, index, p);
}

    int
alcove_decode_longlong(const char *buf, size_t len, int *index, long long *p)
{
    int type = 0;
    int arity = 0;

    if (alcove_get_type(buf, len, index, &type, &arity) < 0)
        return -1;

    return ei_decode_longlong(buf, index, p);
}

    int
alcove_decode_ulonglong(const char *buf, size_t len, int *index, unsigned long long *p)
{
    int type = 0;
    int arity = 0;

    if (alcove_get_type(buf, len, index, &type, &arity) < 0)
        return -1;

    return ei_decode_ulonglong(buf, index, p);
}

    int
alcove_decode_atom(const char *buf, size_t len, int *index, char *p)
{
    int type = 0;
    int arity = 0;

    if (alcove_get_type(buf, len, index, &type, &arity) < 0)
        return -1;

    /* ei_decode_atom will return an error if p >= MAXATOMLEN */
    return ei_decode_atom(buf, index, p);
}

    int
alcove_decode_list_header(const char *buf, size_t len, int *index, int *size)
{
    int type = 0;
    int arity = 0;

    if (alcove_get_type(buf, len, index, &type, &arity) < 0)
        return -1;

    return ei_decode_list_header(buf, index, size);
}

    int
alcove_decode_tuple_header(const char *buf, size_t len, int *index, int *size)
{
    int type = 0;
    int arity = 0;

    if (alcove_get_type(buf, len, index, &type, &arity) < 0)
        return -1;

    return ei_decode_tuple_header(buf, index, size);
}

    int
alcove_decode_iolist(const char *buf, size_t len, int *index,
        char *res, size_t *rlen)
{
    int type = 0;
    int arity = 0;
    int rindex = 0;

    if (alcove_get_type(buf, len, index, &type, &arity) < 0)
        return -1;

    switch (type) {
        case ERL_BINARY_EXT:
        case ERL_LIST_EXT:
        case ERL_NIL_EXT:
        case ERL_STRING_EXT:
            break;

        default:
            return -1;
    }

    if (alcove_decode_iolist_internal(buf, len, index,
                res, *rlen, &rindex, 0) < 0)
        return -1;

    *rlen = rindex;

    return 0;
}

    static int
alcove_decode_iolist_internal(const char *buf, size_t len, int *index,
        char *res, size_t rlen, int *rindex, int depth)
{
    int type = 0;
    int arity = 0;

    /* Arbitrary depth to avoid stack overflows */
    if (depth > 16)
        return -1;

    if (alcove_get_type(buf, len, index, &type, &arity) < 0)
        return -1;

    switch (type) {
        case ERL_STRING_EXT:
            if (*rindex + arity + 1 > rlen)
                return -1;

            if (ei_decode_string(buf, index, res + *rindex) < 0)
                return -1;

            /* Do not include trailing NULL */
            *rindex += arity;
            break;

        case ERL_BINARY_EXT: {
            long int length = 0;

            if (*rindex + arity > rlen)
                return -1;

            if (ei_decode_binary(buf, index, res + *rindex, &length) < 0)
                return -1;

            *rindex += length;
            }
            break;

        case ERL_SMALL_INTEGER_EXT: {
            unsigned long p = 0;

            if (*rindex + 1 > rlen)
                return -1;

            if (ei_decode_ulong(buf, index, &p) < 0)
                return -1;

            res[*rindex] = p;
            *rindex += 1;
            }
            break;

        case ERL_NIL_EXT:
            if (ei_decode_list_header(buf, index, &arity) < 0)
                return -1;
            break;

        case ERL_LIST_EXT: {
            int i = 0;
            int length = 0;

            if (ei_decode_list_header(buf, index, &length) < 0)
                return -1;

            for (i = 0; i < length; i++) {
                if (*rindex >= rlen)
                    return -1;
                if (alcove_decode_iolist_internal(buf, len, index,
                            res, rlen, rindex, depth + 1) < 0)
                    return -1;
            }

            /* [] */
            if (alcove_decode_list_header(buf, len, index, &length) < 0
                    || length != 0)
                return -1;

            }
            break;

        default:
            return -1;
    }

    return 0;
}

    static char *
alcove_x_decode_iolist_to_string(const char *buf, size_t len, int *index)
{
    char tmp[MAXMSGLEN] = {0};
    size_t tmplen = sizeof(tmp) - 1;
    char *res = NULL;

    if (alcove_decode_iolist(buf, len, index, tmp, &tmplen) < 0)
        return NULL;

    res = strdup(tmp);
    if (res == NULL)
        err(errno, "strdup");

    return res;
}

    int
alcove_decode_define(const char *buf, size_t len, int *index, int *val,
        const alcove_define_t *constants)
{
    int type = 0;
    int arity = 0;

    char define[MAXATOMLEN] = {0};

    union {
        int i;
        unsigned long long ull;
    } constant;

    if (alcove_get_type(buf, len, index, &type, &arity) < 0)
        return -1;

    switch (type) {
        case ERL_ATOM_EXT:
            if (alcove_decode_atom(buf, len, index, define) < 0)
                return -1;

            if (alcove_lookup_define(define, &constant.ull, constants) < 0)
                return 1;

            *val = constant.i;

            break;

        case ERL_SMALL_INTEGER_EXT:
        case ERL_INTEGER_EXT:
            if (alcove_decode_int(buf, len, index, val) < 0)
                return -1;
            break;

        default:
            return -1;
    }

    return 0;
}

    int
alcove_decode_define_list(const char *buf, size_t len, int *index, int *val,
        const alcove_define_t *constants)
{
    int type = 0;
    int arity = 0;

    if (alcove_get_type(buf, len, index, &type, &arity) < 0)
        return -1;

    switch (type) {
        case ERL_NIL_EXT:
            if (ei_decode_list_header(buf, index, &arity) < 0)
                return -1;
            *val = 0;
            break;

        case ERL_SMALL_INTEGER_EXT:
        case ERL_INTEGER_EXT:
            if (alcove_decode_int(buf, len, index, val) < 0)
                return -1;
            break;

        case ERL_STRING_EXT: {
            char tmp[MAXMSGLEN] = {0};
            char *p = tmp;

            if (arity >= sizeof(tmp))
                return -1;

            if (ei_decode_string(buf, index, tmp) < 0)
                return -1;

            for ( ; *p; p++)
                *val |= *p;

            }
            break;

        case ERL_LIST_EXT: {
            int i = 0;
            int length = 0;
            int constant = 0;
            int rv = 0;

            if (ei_decode_list_header(buf, index, &length) < 0)
                return -1;

            for (i = 0; i < length; i++) {
                rv = alcove_decode_define(buf, len, index, &constant,
                        constants);

                if (rv != 0)
                    return rv;

                *val |= constant;
            }

            /* [] */
            if (alcove_decode_list_header(buf, len, index, &length) < 0
                    || length != 0)
                return -1;

            }
            break;

        default:
            return -1;
    }

    return 0;
}

    static void *
alcove_malloc(ssize_t size)
{
    void *buf = NULL;

    if (size <= 0 || size >= INT32_MAX)
        errx(ENOMEM, "malloc:invalid size:%ld",
                (unsigned long)size);

    buf = malloc(size);

    if (buf == NULL)
        err(ENOMEM, "malloc");

    return buf;
}

    int
alcove_decode_list_to_argv(const char *arg, size_t len, int *index,
        char ***argv)
{
    int arity = 0;
    int empty = 0;

    int i = 0;
    long maxarg = sysconf(_SC_ARG_MAX);

    if (alcove_decode_list_header(arg, len, index, &arity) < 0)
        return -1;

    if (arity < 0 || arity >= maxarg)
        return -1;

    /* NULL terminate */
    *argv = calloc(arity + 1, sizeof(char *));

    if (*argv == NULL)
        err(errno, "calloc");

    for (i = 0; i < arity; i++) {
        (*argv)[i] = alcove_x_decode_iolist_to_string(arg, len, index);
        if (!(*argv)[i])
            goto BADARG;
    }

    /* list tail */
    if (arity > 0 && (alcove_decode_list_header(arg, len, index, &empty) < 0
                || empty != 0))
        goto BADARG;

    return 0;

BADARG:
    alcove_free_argv(*argv);
    return -1;
}

    void
alcove_free_argv(char **argv)
{
    int i = 0;

    if (argv == NULL)
        return;

    for (i = 0; argv[i]; i++) {
        free(argv[i]);
        argv[i] = NULL;
    }

    free(argv);
    argv = NULL;
}

    int
alcove_decode_list_to_buf(const char *arg, size_t len, int *index,
        char *res, size_t *rlen, alcove_alloc_t **ptr, ssize_t *nptr)
{
    int type = 0;
    int arity = 0;
    long size = 0;

    int tmp_index = 0;
    int tmp_arity = 0;

    char tmp[MAXMSGLEN] = {0};
    unsigned long val = 0;

    int i = 0;
    size_t n = 0;

    *nptr = 0;

    if (alcove_decode_list_header(arg, len, index, &arity) < 0)
        return -1;

    if (arity < 0 || arity >= MAXMSGLEN)
        return -1;

    tmp_index = *index;
    tmp_arity = arity;

    /* Calculate the size required */
    for (i = 0; i < arity; i++) {
        if (alcove_get_type(arg, len, &tmp_index, &type, &tmp_arity) < 0)
            return -1;

        switch (type) {
            case ERL_BINARY_EXT:
                if (tmp_arity > sizeof(tmp))
                    return -1;

                if (ei_decode_binary(arg, &tmp_index, tmp, &size) < 0)
                    return -1;

                n += size;
                break;

            case ERL_SMALL_TUPLE_EXT:
            case ERL_LARGE_TUPLE_EXT:
                if (tmp_arity != 2)
                    return -1;

                if (ei_decode_tuple_header(arg, &tmp_index, &tmp_arity) < 0)
                    return -1;

                if (ei_decode_atom(arg, &tmp_index, tmp) < 0)
                    return -1;

                if (strcmp(tmp, "ptr") != 0)
                    return -1;

                if (ei_get_type(arg, &tmp_index, &type, &tmp_arity) < 0)
                    return -1;

                switch (type) {
                    case ERL_SMALL_INTEGER_EXT:
                    case ERL_INTEGER_EXT:
                        if (ei_decode_ulong(arg, &tmp_index, &val) < 0 ||
                                val > MAXMSGLEN)
                            return -1;

                        n += sizeof(void *);
                        break;

                    case ERL_BINARY_EXT:
                        if (ei_decode_binary(arg, &tmp_index, tmp, &size) < 0
                                || size > MAXMSGLEN)
                            return -1;

                        n += sizeof(void *);
                        break;

                    default:
                        return -1;
                }
                break;

            default:
                return -1;
        }
    }

    if (n > *rlen)
        return -1;

    *rlen = n;

    *ptr = alcove_malloc(arity * sizeof(alcove_alloc_t));
    *nptr = arity;

    /* Copy the list contents */
    for (i = 0; i < arity; i++) {
        (void)ei_get_type(arg, index, &type, &tmp_arity);

        switch (type) {
            case ERL_BINARY_EXT:
                (void)ei_decode_binary(arg, index, res, &size);
                res += size;
                (*ptr)[i].p = NULL;
                (*ptr)[i].len = size;
                break;

            case ERL_SMALL_TUPLE_EXT:
            case ERL_LARGE_TUPLE_EXT:
                (void)ei_decode_tuple_header(arg, index, &tmp_arity);
                (void)ei_decode_atom(arg, index, tmp);
                (void)ei_get_type(arg, index, &type, &tmp_arity);

                switch (type) {
                    case ERL_SMALL_INTEGER_EXT:
                    case ERL_INTEGER_EXT: {
                        char *p = NULL;

                        (void)ei_decode_ulong(arg, index, &val);

                        p = calloc(val, 1);
                        if (p == NULL)
                            err(errno, "calloc");

                        (void)memcpy(res, &p, sizeof(void *));
                        res += sizeof(void *);
                        (*ptr)[i].p = p;
                        (*ptr)[i].len = val;
                        }
                        break;

                    case ERL_BINARY_EXT: {
                        char *p = NULL;
                        (void)ei_decode_binary(arg, index, tmp, &size);
                        p = alcove_malloc(size);
                        (void)memcpy(p, tmp, size);
                        (void)memcpy(res, &p, sizeof(void *));
                        res += sizeof(void *);
                        (*ptr)[i].p = p;
                        (*ptr)[i].len = size;
                        }
                        break;
                }
        }
    }

    return 0;
}

    int
alcove_get_type(const char *buf, size_t len, const int *index,
        int *type, int *arity)
{
    const char *s = buf + *index;
    int n = *index + 1;

    if (*index < 0 || *index >= MAXMSGLEN || *index >= len)
        return -1;

    *type = get_int8(s);
    s += 1;

    switch (*type) {
        case ERL_SMALL_ATOM_EXT:
        case ERL_SMALL_ATOM_UTF8_EXT:
            *type = ERL_ATOM_EXT;
        case ERL_SMALL_TUPLE_EXT:
            n += 1;
            if (n > len)
                return -1;

            *arity = get_int8(s);
            break;

        case ERL_ATOM_UTF8_EXT:
            *type = ERL_ATOM_EXT;
        case ERL_ATOM_EXT:
        case ERL_STRING_EXT:
            n += 2;
            if (n > len)
                return -1;

            *arity = get_int16(s);
            break;

        case ERL_LARGE_TUPLE_EXT:
        case ERL_LIST_EXT:
        case ERL_BINARY_EXT:
            n += 4;
            if (n > len)
                return -1;

            *arity = get_int32(s);
            break;

        case ERL_SMALL_BIG_EXT:
            n += 1;
            if (n > len)
                return -1;

            *arity = get_int8(s);
            break;

        case ERL_LARGE_BIG_EXT:
            n += 4;
            if (n > len)
                return -1;

            *arity = get_int32(s);
            break;

        case ERL_SMALL_INTEGER_EXT:
            n += 1;
            if (n > len)
                return -1;

            *arity = 0;
            break;

        case ERL_INTEGER_EXT:
            n += 4;
            if (n > len)
                return -1;

            *arity = 0;
            break;

        case ERL_NIL_EXT:
            *arity = 0;
            break;

        default:
            return -1;
    }

    return 0;
}
