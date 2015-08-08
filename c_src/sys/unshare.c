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
#include "alcove_fork.h"
#include "alcove_clone_constants.h"

/*
 * unshare(2)
 *
 */
    ssize_t
alcove_sys_unshare(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
#ifdef __linux__
    int index = 0;
    int flags = 0;

    /* flags */
    switch (alcove_decode_define_list(arg, len, &index, &flags,
                alcove_clone_constants)) {
        case 0:
            break;
        case 1:
            return alcove_mk_error(reply, rlen, "unsupported");
        default:
            return -1;
    }

    return (unshare(flags) < 0)
        ? alcove_mk_errno(reply, rlen, errno)
        : alcove_mk_atom(reply, rlen, "ok");
#else
    return alcove_mk_atom(reply, rlen, "undef");
#endif
}
