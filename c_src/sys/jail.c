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

#if defined(__FreeBSD__)
#include <sys/jail.h>

void alcove_free_cstruct(alcove_alloc_t *ptr, ssize_t nptr);
#endif

/*
 * jail(2)
 *
 */
    ssize_t
alcove_sys_jail(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
#if defined(__FreeBSD__)
    int index = 0;
    int rindex = 0;

    struct jail j = {0};
    size_t jlen = sizeof(j);

    alcove_alloc_t *elem = NULL;
    ssize_t nelem = 0;

    int jid = 0;

    if (alcove_decode_cstruct(arg, len, &index, (char *)&j, &jlen,
                &elem, &nelem) < 0)
        return -1;

    jid = jail(&j);

    alcove_free_cstruct(elem, nelem);

    if (jid < 0)
        return alcove_mk_errno(reply, rlen, errno);

    ALCOVE_OK(
            reply,
            rlen,
            &rindex,
            alcove_encode_long(reply, rlen, &rindex, jid)
            );

    return rindex;
#else
    return alcove_mk_atom(reply, rlen, "undef");
#endif
}

#if defined(__FreeBSD__)
    void
alcove_free_cstruct(alcove_alloc_t *ptr, ssize_t nptr)
{
    int i = 0;

    for (i = 0; i < nptr; i++) {
        if (ptr[i].p)
            free(ptr[i].p);
    }

    free(ptr);
}
#endif
