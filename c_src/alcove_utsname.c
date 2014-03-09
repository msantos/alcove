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

#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <err.h>
#include <errno.h>

#include <sys/types.h>

#ifndef HOST_NAME_MAX
#define HOST_NAME_MAX MAXHOSTNAMELEN
#endif

/*
 * gethostname(2)
 *
 */
    ETERM *
alcove_gethostname(alcove_state_t *ap, ETERM *arg)
{
    char name[HOST_NAME_MAX] = {0};
    int rv = 0;

    rv = gethostname(name, HOST_NAME_MAX-1);

    return (rv < 0)
        ? alcove_errno(errno)
        : alcove_ok(erl_mk_binary(name, strlen(name)));
}

/*
 * sethostname(2)
 *
 */
    ETERM *
alcove_sethostname(alcove_state_t *ap, ETERM *arg)
{
    ETERM *hd = NULL;
    char *name = NULL;
    int rv = 0;

    /* hostname */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ALCOVE_IS_IOLIST(hd))
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        name = erl_iolist_to_string(hd);

    if (!name)
        goto BADARG;

    rv = sethostname(name, strlen(name)+1);

    erl_free(name);

    return ( (rv < 0) ? alcove_errno(errno) : erl_mk_atom("ok"));

BADARG:
    erl_free(name);
    return erl_mk_atom("badarg");
}
