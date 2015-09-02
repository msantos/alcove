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

static int count_pid(alcove_state_t *ap, alcove_child_t *c,
        void *arg1, void *arg2);
static int cons_pid(alcove_state_t *ap, alcove_child_t *c,
        void *arg1, void *arg2);

    ssize_t
alcove_sys_pid(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int rindex = 0;
    size_t count = 0;

    /* Count the number of PIDs */
    if (pid_foreach(ap, 0, &count, NULL, pid_not_equal, count_pid) < 0)
        return -1;

    /* Ensure the buffer is large enough to hold the PIDs
     * version = 1 byte
     *     list header = 5 bytes
     *     tuple header = 5 bytes
     *     atom = 1 byte
     *     alcove_pid = 10 bytes
     *     4 * int (7) = 28 bytes
     * tail = 1 byte
     *
     * length = 1 + (count * 49) + 1
     */
    if (2 + count * 49 > rlen)
        return -1;

    if (ei_encode_version(reply, &rindex) < 0)
        return -1;

    if (pid_foreach(ap, 0, reply, &rindex, pid_not_equal, cons_pid) < 0)
        return -1;

    if (ei_encode_empty_list(reply, &rindex) < 0)
        return -1;

    return rindex;
}

    static int
count_pid(alcove_state_t *ap, alcove_child_t *c, void *arg1, void *arg2)
{
    size_t *n = arg1;
    *n += 1;
    return 1;
}

    static int
cons_pid(alcove_state_t *ap, alcove_child_t *c, void *arg1, void *arg2)
{
    char *buf = arg1;
    int *index = arg2;

    if (ei_encode_list_header(buf, index, 1) < 0)
        return -1;

    if (ei_encode_tuple_header(buf, index, 6) < 0)
        return -1;

    if (ei_encode_atom(buf, index, "alcove_pid") < 0)
        return -1;

    if (ei_encode_long(buf, index, c->pid) < 0)
        return -1;

    if (ei_encode_long(buf, index, c->fdctl) < 0)
        return -1;

    if (ei_encode_long(buf, index, c->fdin) < 0)
        return -1;

    if (ei_encode_long(buf, index, c->fdout) < 0)
        return -1;

    if (ei_encode_long(buf, index, c->fderr) < 0)
        return -1;

    return 1;
}
