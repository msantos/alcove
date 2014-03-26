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

#include <sys/mount.h>
#include "alcove_mount.h"


/*
 * mount(2)
 *
 */
    ETERM *
alcove_mount(alcove_state_t *ap, ETERM *arg)
{
    ETERM *hd = NULL;
    char *source = NULL;
    char *target = NULL;
    char *filesystemtype = NULL;
    unsigned long mountflags = 0;
    void *data = NULL;

    int rv = 0;

    /* source */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ALCOVE_IS_IOLIST(hd))
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        source = erl_iolist_to_string(hd);

    if (!source)
        goto BADARG;

    /* target */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ALCOVE_IS_IOLIST(hd))
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        target = erl_iolist_to_string(hd);

    if (!target)
        goto BADARG;

    /* filesystemtype */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ALCOVE_IS_IOLIST(hd))
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        filesystemtype = erl_iolist_to_string(hd);

    /* mountflags */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_INTEGER(hd))
        goto BADARG;

    mountflags = ERL_INT_VALUE(hd);

    /* data */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ALCOVE_IS_IOLIST(hd))
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        data = ERL_BIN_PTR(erl_iolist_to_binary(hd));

#ifdef __linux__
    rv = mount(source, target, filesystemtype, mountflags, data);
#else
    rv = mount(filesystemtype, target, mountflags, data);
#endif

    erl_free(source);
    erl_free(target);
    erl_free(filesystemtype);
    erl_free(data);

    return ( (rv < 0) ? alcove_errno(errno) : erl_mk_atom("ok"));

BADARG:
    erl_free(source);
    erl_free(target);
    erl_free(filesystemtype);
    erl_free(data);
    return erl_mk_atom("badarg");
}

/*
 * umount(2)
 *
 */
    ETERM *
alcove_umount(alcove_state_t *ap, ETERM *arg)
{
    ETERM *hd = NULL;
    char *source = NULL;

    int rv = 0;

    /* source */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ALCOVE_IS_IOLIST(hd))
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        source = erl_iolist_to_string(hd);

    if (!source)
        goto BADARG;

#ifdef __linux__
    rv = umount(source);
#else
    rv = unmount(source, 0);
#endif

    erl_free(source);

    return ( (rv < 0) ? alcove_errno(errno) : erl_mk_atom("ok"));

BADARG:
    erl_free(source);
    return erl_mk_atom("badarg");
}

/*
 * mount constants
 *
 */
    ETERM *
alcove_mount_define(alcove_state_t *ap, ETERM *arg)
{
    ETERM *hd = NULL;
    char *name = NULL;

    /* flag */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_ATOM(hd))
        goto BADARG;

    name = ERL_ATOM_PTR(hd);

    return alcove_define(name, alcove_mount_constants);

BADARG:
    return erl_mk_atom("badarg");
}
