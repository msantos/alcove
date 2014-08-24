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

extern char **environ;

/*
 * environ(7)
 *
 */
    ssize_t
alcove_environ(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int rindex = 0;
    char **envp = environ;

    ALCOVE_ERR(alcove_encode_version(reply, rlen, &rindex));

    for ( ; envp && *envp; envp++) {
        ALCOVE_ERR(alcove_encode_list_header(reply, rlen, &rindex, 1));
        ALCOVE_ERR(alcove_encode_binary(reply, rlen, &rindex, *envp, strlen(*envp)));
    }

    ALCOVE_ERR(alcove_encode_empty_list(reply, rlen, &rindex));

    return rindex;
}

/*
 * getenv(3)
 *
 */
    ssize_t
alcove_getenv(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;
    char name[MAXMSGLEN] = {0};
    size_t namelen = sizeof(name)-1;
    char *value = NULL;

    /* name */
    if (alcove_decode_iolist(arg, len, &index, name, &namelen) < 0 ||
            namelen == 0)
        return -1;

    value = getenv(name);

    return value
        ? alcove_mk_binary(reply, rlen, value, strlen(value))
        : alcove_mk_atom(reply, rlen, "false");
}

/*
 * setenv(3)
 *
 */

    ssize_t
alcove_setenv(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;
    char name[MAXMSGLEN] = {0};
    size_t namelen = sizeof(name)-1;
    char value[MAXMSGLEN] = {0};
    size_t valuelen = sizeof(value)-1;
    int overwrite = 0;
    int rv = 0;

    /* name */
    if (alcove_decode_iolist(arg, len, &index, name, &namelen) < 0 ||
            namelen == 0)
        return -1;

    /* value */
    if (alcove_decode_iolist(arg, len, &index, value, &valuelen) < 0)
        return -1;

    /* overwrite */
    if (alcove_decode_int(arg, len, &index, &overwrite) < 0)
        return -1;

    rv = setenv(name, value, overwrite);

    return (rv < 0)
        ? alcove_errno(reply, rlen, errno)
        : alcove_mk_atom(reply, rlen, "ok");
}

/*
 * unsetenv(3)
 *
 */
    ssize_t
alcove_unsetenv(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;
    char name[MAXMSGLEN] = {0};
    size_t namelen = sizeof(name)-1;
    int rv = 0;

    /* name */
    if (alcove_decode_iolist(arg, len, &index, name, &namelen) < 0 ||
            namelen == 0)
        return -1;

    rv = unsetenv(name);

    return (rv < 0)
        ? alcove_errno(reply, rlen, errno)
        : alcove_mk_atom(reply, rlen, "ok");
}

/*
 * clearenv(3)
 *
 */
    ssize_t
alcove_clearenv(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
#ifdef __linux__
    int rv = 0;

    rv = clearenv();

    return (rv < 0)
        ? alcove_errno(reply, rlen, errno)
        : alcove_mk_atom(reply, rlen, "ok");
#else
    environ = NULL;
    return alcove_mk_atom(reply, rlen, "ok");
#endif
}
