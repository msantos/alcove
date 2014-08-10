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
#include "alcove_limit.h"

/*
 * getrlimit(2)
 *
 */
    ssize_t
alcove_getrlimit(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;
    int rindex = 0;

    int resource = 0;
    struct rlimit rlim = {0};
    int rv = 0;

    /* resource */
    if (alcove_decode_int(arg, &index, &resource) < 0)
        return -1;

    rv = getrlimit(resource, &rlim);

    if (rv < 0)
        return  alcove_errno(reply, rlen, errno);

    ALCOVE_ERR(ei_encode_version(reply, &rindex));
    ALCOVE_ERR(ei_encode_tuple_header(reply, &rindex, 2));
    ALCOVE_ERR(ei_encode_atom(reply, &rindex, "ok"));
    ALCOVE_ERR(ei_encode_tuple_header(reply, &rindex, 3));
    ALCOVE_ERR(ei_encode_atom(reply, &rindex, "alcove_rlimit"));
    ALCOVE_ERR(ei_encode_ulonglong(reply, &rindex, rlim.rlim_cur));
    ALCOVE_ERR(ei_encode_ulonglong(reply, &rindex, rlim.rlim_max));

    return rindex;
}

/*
 * setrlimit(2)
 *
 */
    ssize_t
alcove_setrlimit(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;
    int arity = 0;

    int resource = 0;
    char atom[MAXATOMLEN] = {0};
    unsigned long long cur = 0, max = 0;
    struct rlimit rlim = {0};
    int rv = 0;

    /* resource */
    if (alcove_decode_int(arg, &index, &resource) < 0)
        return -1;

    /* {alcove_rlimit, rlim_cur, rlim_max} */
    if (ei_decode_tuple_header(arg, &index, &arity) < 0 || arity != 3)
        return -1;

    /* 'alcove_rlimit' */
    if (ei_decode_atom(arg, &index, atom) < 0 ||
            strncmp(atom, "alcove_rlimit", 13))
        return -1;

    /* rlim_cur: soft limit */
    if (ei_decode_ulonglong(arg, &index, &cur) < 0)
        return -1;

    /* rlim_max: hard limit */
    if (ei_decode_ulonglong(arg, &index, &max) < 0)
        return -1;

    rlim.rlim_cur = cur;
    rlim.rlim_max = max;

    rv = setrlimit(resource, &rlim);

    return (rv < 0)
        ? alcove_errno(reply, rlen, errno)
        : alcove_mk_atom(reply, rlen, "ok");
}

/*
 * rlimit constants
 *
 */
    ssize_t
alcove_rlimit_define(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;
    int rindex = 0;

    char name[MAXATOMLEN] = {0};

    /* name */
    if (ei_decode_atom(arg, &index, name) < 0)
        return -1;

    ALCOVE_ERR(ei_encode_version(reply, &rindex));
    ALCOVE_ERR(alcove_define(reply, &rindex, name, alcove_rlimit_constants));
    return rindex;
}
