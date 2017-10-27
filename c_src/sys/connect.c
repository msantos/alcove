/* Copyright (c) 2017, Michael Santos <michael.santos@gmail.com>
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

#include <sys/types.h>
#include <sys/socket.h>

/*
 * connect(2)
 *
 */
    ssize_t
alcove_sys_connect(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;

    int fd = 0;
    struct sockaddr_storage sa = {0};
	size_t salen = sizeof(sa);
    alcove_alloc_t *elem = NULL;
    ssize_t nelem = 0;

    int rv = 0;

    UNUSED(ap);

    /* fd */
    if (alcove_decode_int(arg, len, &index, &fd) < 0)
        return -1;

    /* struct sockaddr */
    if (alcove_decode_cstruct(arg, len, &index, (char *)&sa, &salen,
			&elem, &nelem) < 0)
        return -1;

	rv = connect(fd, (salen == 0 ? NULL : (struct sockaddr *)&sa), salen);

    return (rv < 0)
		? alcove_mk_errno(reply, rlen, errno)
        : alcove_mk_atom(reply, rlen, "ok");
}
