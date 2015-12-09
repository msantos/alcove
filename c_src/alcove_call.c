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
#include "alcove_calls.h"

    ssize_t
alcove_call(alcove_state_t *ap, u_int32_t call,
        const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;
    int arity = 0;
    int version = 0;

    const alcove_call_t *fun = NULL;
    ssize_t written = 0;

    if (call >= sizeof(calls)/sizeof(calls[0]))
        goto BADARG;

    fun = &calls[call];

    /* minimum input in external term format
     * Magic:1/bytes, SmallTupleHeader:1/bytes, Arity:1/bytes
     * <<131,104,0>>
     */
    if (len < 4)
        goto BADARG;

    if (ei_decode_version(arg, &index, &version) < 0)
        goto BADARG;

    if (alcove_decode_tuple_header(arg, len, &index, &arity) < 0)
        goto BADARG;

    if (arity != fun->narg)
        goto BADARG;

    written = (*fun->fp)(ap, arg+index, len-index, reply, rlen);

    if (written < 0)
        goto BADARG;

    return written;

BADARG:
    return alcove_mk_atom(reply, rlen, "badarg");
}
