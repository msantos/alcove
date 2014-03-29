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

/*
 * getenv(3)
 *
 */
    ETERM *
alcove_getenv(alcove_state_t *ap, ETERM *arg)
{
    ETERM *hd = NULL;
    char *name = NULL;
    char *value = NULL;

    /* name */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ALCOVE_IS_IOLIST(hd))
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        name = erl_iolist_to_string(hd);

    if (!name)
        goto BADARG;

    value = getenv(name);

    erl_free(name);

    return value
        ? erl_mk_binary(value, strlen(value))
        : erl_mk_atom("false");

BADARG:
    erl_free(name);
    return erl_mk_atom("badarg");
}

/*
 * setenv(3)
 *
 */

    ETERM *
alcove_setenv(alcove_state_t *ap, ETERM *arg)
{
    ETERM *hd = NULL;
    char *name = NULL;
    char *value = NULL;
    int overwrite = 0;
    int rv = 0;
    int errnum = 0;

    /* name */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ALCOVE_IS_IOLIST(hd))
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        name = erl_iolist_to_string(hd);

    if (!name)
        goto BADARG;

    /* value */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ALCOVE_IS_IOLIST(hd))
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        value = erl_iolist_to_string(hd);

    if (!value)
        goto BADARG;

    /* overwrite */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_INTEGER(hd))
        goto BADARG;

    overwrite = ERL_INT_VALUE(hd);

    rv = setenv(name, value, overwrite);

    errnum = errno;

    erl_free(name);
    erl_free(value);

    return (rv < 0)
        ? alcove_errno(errnum)
        : erl_mk_atom("ok");

BADARG:
    erl_free(name);
    erl_free(value);
    return erl_mk_atom("badarg");
}

/*
 * unsetenv(3)
 *
 */
    ETERM *
alcove_unsetenv(alcove_state_t *ap, ETERM *arg)
{
    ETERM *hd = NULL;
    char *name = NULL;
    int rv = 0;
    int errnum = 0;

    /* name */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ALCOVE_IS_IOLIST(hd))
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        name = erl_iolist_to_string(hd);

    if (!name)
        goto BADARG;

    rv = unsetenv(name);

    erl_free(name);

    return (rv < 0)
        ? alcove_errno(errnum)
        : erl_mk_atom("ok");

BADARG:
    erl_free(name);
    return erl_mk_atom("badarg");
}
