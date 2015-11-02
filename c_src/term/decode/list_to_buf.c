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

    if (arity == 0 || arity >= MAXMSGLEN)
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
                (void)ei_decode_tuple_header(arg, index, &tmp_arity);
                (void)ei_decode_atom(arg, index, tmp);
                (void)ei_get_type(arg, index, &type, &tmp_arity);

                switch (type) {
                    case ERL_SMALL_INTEGER_EXT:
                    case ERL_INTEGER_EXT: {
                        char *p = NULL;

                        (void)ei_decode_ulong(arg, index, &val);

                        if (val > 0) {
                        p = calloc(val, 1);
                        if (p == NULL)
                            err(errno, "calloc");
                        (*ptr)[i].len = val;
                        }
                        else {
                        /* NULL pointer: return a binary */
                        (*ptr)[i].len = sizeof(void *);
                        }
                        (void)memcpy(res, &p, sizeof(void *));
                        res += sizeof(void *);
                        (*ptr)[i].p = p;
                        }
                        break;

                    case ERL_BINARY_EXT: {
                        char *p = NULL;
                        (void)ei_decode_binary(arg, index, tmp, &size);
                        if (size > 0) {
                        p = alcove_malloc(size);
                        (void)memcpy(p, tmp, size);
                        (*ptr)[i].len = size;
                        }
                        else {
                        /* NULL pointer: return a binary */
                        (*ptr)[i].len = sizeof(void *);
                        }
                        (void)memcpy(res, &p, sizeof(void *));
                        res += sizeof(void *);
                        (*ptr)[i].p = p;
                        }
                        break;
                }
        }
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

