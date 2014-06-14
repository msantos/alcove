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

/* Probably only useful for testing */
    ETERM *
alcove_alloc(alcove_state_t *ap, ETERM *arg)
{
    ETERM *hd = NULL;
    char *buf = NULL;
    size_t len = 0;
    size_t buflen = 0;
    alcove_alloc_t *ptr = NULL;
    ssize_t nptr = 0;
    ETERM *t = NULL;

    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_LIST(hd))
        goto BADARG;

    buf = alcove_list_to_buf(hd, &len, &ptr, &nptr);

    if (!buf)
        return erl_mk_atom("badarg");

    buflen = len;

    t = erl_mk_empty_list();
    for (nptr-- ; nptr >= 0; nptr--) {
        if (ptr[nptr].p) {
            /* Allocated buffer */
            len -= sizeof(void *);
            t = erl_cons(alcove_tuple2(
                        erl_mk_atom("ptr"),
                        erl_mk_binary(ptr[nptr].p, ptr[nptr].len)
                        ), t);
            free(ptr[nptr].p);
        }
        else {
            /* Static binary */
            len -= ptr[nptr].len;
            t = erl_cons(erl_mk_binary(buf+len, ptr[nptr].len), t);
        }
    }

    return alcove_tuple3(
            erl_mk_atom("ok"),
            erl_mk_binary(buf, buflen),
            t
            );

BADARG:
    return erl_mk_atom("badarg");
}
