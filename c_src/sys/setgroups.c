/* Copyright (c) 2015, Michael Santos <michael.santos@gmail.com>
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
#include "grp.h"
#endif

static int alcove_list_to_groups(const char *arg, size_t len, int *index,
        int type, gid_t *list, int arity);

/*
 * setgroups(2)
 *
 */
    ssize_t
alcove_sys_setgroups(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;
    gid_t *list = NULL;
    int n;

    int type = 0;
    int arity = 0;

    UNUSED(ap);

    if (alcove_get_type(arg, len, &index, &type, &arity) < 0)
        return -1;

    if (arity > 0) {
        list = calloc(arity, sizeof(gid_t));
        if (list == NULL)
            return alcove_mk_errno(reply, rlen, errno);
    }

    if (alcove_list_to_groups(arg, len, &index, type, list, arity) < 0) {
        free(list);
        return -1;
    }

    n = setgroups(arity, list);

    if (n < 0) {
        free(list);
        return alcove_mk_errno(reply, rlen, errno);
    }

    free(list);

    return alcove_mk_atom(reply, rlen, "ok");
}

    static int
alcove_list_to_groups(const char *arg, size_t len, int *index,
        int type, gid_t *list, int arity)
{
    int n = 0;

    switch (type) {
        case ERL_STRING_EXT: {
            char *tmp = NULL;

            tmp = calloc(arity+1, 1);
            if (tmp == NULL)
                return -1;

            if (alcove_decode_string(arg, len, index, tmp, arity+1) < 0) {
                free(tmp);
                return -1;
            }

            for (n = 0; n < arity; n++)
                list[n] = tmp[n];

            free(tmp);
            }
            break;

        case ERL_LIST_EXT:
            if ( (alcove_decode_list_header(arg, len, index, &n) < 0)
                    || n != arity)
                return -1;

            n--;
            for ( ; n >= 0; n--) {
                gid_t gid = 0;

                if (alcove_decode_uint(arg, len, index, &gid) < 0)
                    return -1;

                list[n] = gid;
            }

            /* ignore the list tail */
            break;

        case ERL_NIL_EXT:
            break;

        default:
            return -1;
    }

    return 0;
}
