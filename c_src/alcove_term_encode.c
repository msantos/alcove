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
#include <ctype.h>

static int alcove_encode_atom_to_lower(char *buf, size_t len, int *index,
        const char *p);

    ssize_t
alcove_mk_errno(char *buf, size_t len, int errnum)
{
    return alcove_mk_error(buf, len, erl_errno_id(errnum));
}

    ssize_t
alcove_mk_error(char *buf, size_t len, const char *reason)
{
    int index = 0;

    if (alcove_encode_version(buf, len, &index) < 0)
        return -1;

    if (alcove_encode_tuple_header(buf, len, &index, 2) < 0)
        return -1;

    if (alcove_encode_atom(buf, len, &index, "error") < 0)
        return -1;

    if (alcove_encode_atom(buf, len, &index, reason) < 0)
        return -1;

    return index;
}

    ssize_t
alcove_mk_atom(char *buf, size_t len, const char *atom)
{
    int index = 0;

    if (alcove_encode_version(buf, len, &index) < 0)
        return -1;

    if (alcove_encode_atom(buf, len, &index, atom) < 0)
        return -1;

    return index;
}

    ssize_t
alcove_mk_binary(char *buf, size_t len, const void *p, size_t plen)
{
    int index = 0;

    if (alcove_encode_version(buf, len, &index) < 0)
        return -1;

    if (alcove_encode_binary(buf, len, &index, p, plen) < 0)
        return -1;

    return index;
}

    ssize_t
alcove_mk_long(char *buf, size_t len, long n)
{
    int index = 0;

    if (alcove_encode_version(buf, len, &index) < 0)
        return -1;

    if (alcove_encode_long(buf, len, &index, n) < 0)
        return -1;

    return index;
}

    ssize_t
alcove_mk_ulong(char *buf, size_t len, unsigned long n)
{
    int index = 0;

    if (alcove_encode_version(buf, len, &index) < 0)
        return -1;

    if (alcove_encode_ulong(buf, len, &index, n) < 0)
        return -1;

    return index;
}

    int
alcove_encode_define(char *buf, size_t len, int *index, char *name,
        const alcove_define_t *constants)
{
    unsigned long long val = 0;

    if (alcove_lookup_define(name, &val, constants) < 0)
        return alcove_encode_atom(buf, len, index, "unknown");

    return alcove_encode_ulonglong(buf, len, index, val);
}

    int
alcove_lookup_define(char *name, unsigned long long *val,
        const alcove_define_t *constants)
{
    const alcove_define_t *dp = NULL;

    for (dp = constants; dp->name != NULL; dp++) {
        if (!strcasecmp(name, dp->name)) {
            *val = dp->val;
            return 0;
        }
    }

    return -1;
}

    int
alcove_encode_constant(char *buf, size_t len, int *index, u_int64_t val,
        const alcove_define_t *constants)
{
    const alcove_define_t *dp = NULL;

    for (dp = constants; dp->name != NULL; dp++) {
        if (val == dp->val)
            return alcove_encode_atom_to_lower(buf, len, index, dp->name);
    }

    return alcove_encode_atom(buf, len, index, "unknown");
}

    int
alcove_encode_buf_to_list(char *reply, size_t rlen, int *rindex,
        const char *buf, size_t len,
        alcove_alloc_t *ptr, ssize_t nptr)
{
    int i = 0;
    size_t offset = 0;

    if (alcove_encode_list_header(reply, rlen, rindex, nptr) < 0)
        return -1;

    for ( ; i < nptr; i++) {
        if (ptr[i].p) {
            /* Allocated buffer */
            if (alcove_encode_tuple_header(reply, rlen, rindex, 2) < 0)
                return -1;
            if (alcove_encode_atom(reply, rlen, rindex, "ptr") < 0)
                return -1;
            if (alcove_encode_binary(reply, rlen, rindex, ptr[i].p,
                        ptr[i].len) < 0)
                return -1;
            free(ptr[i].p);
            offset += sizeof(void *);
        }
        else {
            /* Static binary */
            if (alcove_encode_binary(reply, rlen, rindex, buf+offset, ptr[i].len) < 0)
                return -1;
            offset += ptr[i].len;
        }
    }

    if (alcove_encode_empty_list(reply, rlen, rindex) < 0)
        return -1;

    free(ptr);
    ptr = NULL;

    return 0;
}


/* Wrappers around the ei encode functions with length checks */
    int
alcove_encode_version(char *buf, size_t len, int *index)
{
    int n = *index;

    if (*index < 0 || *index > MAXMSGLEN)
        return -1;

    if (ei_encode_version(NULL, &n) < 0)
        return -1;

    if (n > len)
        return -1;

    return ei_encode_version(buf, index);
}

    int
alcove_encode_list_header(char *buf, size_t len, int *index, int arity)
{
    int n = *index;

    if (*index < 0 || *index > MAXMSGLEN)
        return -1;

    if (ei_encode_list_header(NULL, &n, arity) < 0)
        return -1;

    if (n > len)
        return -1;

    return ei_encode_list_header(buf, index, arity);
}

    int
alcove_encode_empty_list(char *buf, size_t len, int *index)
{
    int n = *index;

    if (*index < 0 || *index > MAXMSGLEN)
        return -1;

    if (ei_encode_empty_list(NULL, &n) < 0)
        return -1;

    if (n > len)
        return -1;

    return ei_encode_empty_list(buf, index);
}

    int
alcove_encode_tuple_header(char *buf, size_t len, int *index, int arity)
{
    int n = *index;

    if (*index < 0 || *index > MAXMSGLEN)
        return -1;

    if (ei_encode_tuple_header(NULL, &n, arity) < 0)
        return -1;

    if (n > len)
        return -1;

    return ei_encode_tuple_header(buf, index, arity);
}

    int
alcove_encode_long(char *buf, size_t len, int *index, long x)
{
    int n = *index;

    if (*index < 0 || *index > MAXMSGLEN)
        return -1;

    if (ei_encode_long(NULL, &n, x) < 0)
        return -1;

    if (n > len)
        return -1;

    return ei_encode_long(buf, index, x);
}

    int
alcove_encode_ulong(char *buf, size_t len, int *index, unsigned long x)
{
    int n = *index;

    if (*index < 0 || *index > MAXMSGLEN)
        return -1;

    if (ei_encode_ulong(NULL, &n, x) < 0)
        return -1;

    if (n > len)
        return -1;

    return ei_encode_ulong(buf, index, x);
}

    int
alcove_encode_longlong(char *buf, size_t len, int *index, long long x)
{
    int n = *index;

    if (*index < 0 || *index > MAXMSGLEN)
        return -1;

    if (ei_encode_longlong(NULL, &n, x) < 0)
        return -1;

    if (n > len)
        return -1;

    return ei_encode_longlong(buf, index, x);
}

    int
alcove_encode_ulonglong(char *buf, size_t len, int *index, unsigned long long x)
{
    int n = *index;

    if (*index < 0 || *index > MAXMSGLEN)
        return -1;

    if (ei_encode_ulonglong(NULL, &n, x) < 0)
        return -1;

    if (n > len)
        return -1;

    return ei_encode_ulonglong(buf, index, x);
}

    int
alcove_encode_atom(char *buf, size_t len, int *index, const char *p)
{
    int n = *index;

    if (*index < 0 || *index > MAXMSGLEN)
        return -1;

    if (ei_encode_atom(NULL, &n, p) < 0)
        return -1;

    if (n > len)
        return -1;

    return ei_encode_atom(buf, index, p);
}

    static int
alcove_encode_atom_to_lower(char *buf, size_t len, int *index, const char *p)
{
    char atom[MAXATOMLEN] = {0};
    char *q = atom;

    for ( ; *p; p++, q++)
        *q = tolower((int)(unsigned char)*p);

    return alcove_encode_atom(buf, len, index, atom);
}

    int
alcove_encode_binary(char *buf, size_t len, int *index, const void *p, long plen)
{
    int n = *index;

    if (*index < 0 || *index > MAXMSGLEN)
        return -1;

    if (ei_encode_binary(NULL, &n, p, plen) < 0)
        return -1;

    if (n > len)
        return -1;

    return ei_encode_binary(buf, index, p, plen);
}
