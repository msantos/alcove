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
#include "alcove_prio_constants.h"

/*
 * setpriority(2)
 */
    ssize_t
alcove_sys_setpriority(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;

    int which = 0;
    int who = 0;
    int prio = 0;

    switch (alcove_decode_define(arg, len, &index, &which,
                alcove_prio_constants)) {
        case 0:
            break;

        case 1:
            return alcove_mk_errno(reply, rlen, EINVAL);

        case -1:
        default:
            return -1;
    }

    switch (alcove_decode_define(arg, len, &index, &who,
                alcove_prio_constants)) {
        case 0:
            break;

        case 1:
            return alcove_mk_errno(reply, rlen, EINVAL);

        case -1:
        default:
            return -1;
    }

    if (alcove_decode_int(arg, len, &index, &prio) < 0)
        return -1;

    if (setpriority(which, who, prio) < 0)
        return alcove_mk_errno(reply, rlen, errno);

    return alcove_mk_atom(reply, rlen, "ok");
}
