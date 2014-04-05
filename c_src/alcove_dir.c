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

#include <sys/stat.h>
#include <dirent.h>

/*
 * chdir(2)
 *
 */
    ETERM *
alcove_chdir(alcove_state_t *ap, ETERM *arg)
{
    ETERM *hd = NULL;
    char *path = NULL;
    int rv = 0;
    int errnum = 0;

    /* path */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ALCOVE_IS_IOLIST(hd))
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        path = erl_iolist_to_string(hd);

    if (!path)
        goto BADARG;

    rv = chdir(path);

    errnum = errno;

    erl_free(path);

    return (rv < 0) ? alcove_errno(errnum) : erl_mk_atom("ok");

BADARG:
    erl_free(path);
    return erl_mk_atom("badarg");
}

/*
 * mkdir(2)
 *
 */
    ETERM *
alcove_mkdir(alcove_state_t *ap, ETERM *arg)
{
    ETERM *hd = NULL;
    char *pathname = NULL;
    mode_t mode = {0};
    int rv = 0;
    int errnum = 0;

    /* pathname */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ALCOVE_IS_IOLIST(hd))
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        pathname = erl_iolist_to_string(hd);

    if (!pathname)
        goto BADARG;

    /* mode */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ALCOVE_IS_UNSIGNED_INTEGER(hd))
        goto BADARG;

    mode = ERL_INT_UVALUE(hd);

    rv = mkdir(pathname, mode);

    errnum = errno;

    erl_free(pathname);

    return ( (rv < 0) ? alcove_errno(errnum) : erl_mk_atom("ok"));

BADARG:
    erl_free(pathname);
    return erl_mk_atom("badarg");
}

/*
 * rmdir(2)
 *
 */
    ETERM *
alcove_rmdir(alcove_state_t *ap, ETERM *arg)
{
    ETERM *hd = NULL;
    char *pathname = NULL;
    int rv = 0;
    int errnum = 0;

    /* pathname */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ALCOVE_IS_IOLIST(hd))
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        pathname = erl_iolist_to_string(hd);

    if (!pathname)
        goto BADARG;

    rv = rmdir(pathname);

    errnum = errno;

    erl_free(pathname);

    return (rv < 0) ? alcove_errno(errnum) : erl_mk_atom("ok");

BADARG:
    erl_free(pathname);
    return erl_mk_atom("badarg");
}

/*
 * chroot(2)
 *
 */
    ETERM *
alcove_chroot(alcove_state_t *ap, ETERM *arg)
{
    ETERM *hd = NULL;
    char *path = NULL;
    int rv = 0;
    int errnum = 0;

    /* path */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ALCOVE_IS_IOLIST(hd))
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        path = erl_iolist_to_string(hd);

    if (!path)
        goto BADARG;

    rv = chroot(path);

    errnum = errno;

    erl_free(path);

    return ( (rv < 0) ? alcove_errno(errnum) : erl_mk_atom("ok"));

BADARG:
    erl_free(path);
    return erl_mk_atom("badarg");
}

/*
 * getcwd(3)
 *
 */
    ETERM *
alcove_getcwd(alcove_state_t *ap, ETERM *arg)
{
    char buf[PATH_MAX] = {0};

    if (!getcwd(buf, sizeof(buf)))
        return alcove_errno(errno);

    return alcove_ok(erl_mk_binary(buf, strlen(buf)));
}

/*
 * readdir(3)
 *
 */
    ETERM *
alcove_readdir(alcove_state_t *ap, ETERM *arg)
{
    ETERM *hd = NULL;
    char *name = NULL;
    DIR *dirp = NULL;
    struct dirent *dent = NULL;
    ETERM *t = erl_mk_empty_list();
    int errnum = 0;

    /* name */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ALCOVE_IS_IOLIST(hd))
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        name = erl_iolist_to_string(hd);

    if (!name)
        goto BADARG;

    dirp = opendir(name);

    if (!dirp)
        goto ERR;

    errno = 0;
    while ( (dent = readdir(dirp))) {
        if (!strcmp(dent->d_name, ".") || !strcmp(dent->d_name, ".."))
            continue;

        t = erl_cons(erl_mk_binary(dent->d_name, strlen(dent->d_name)), t);
    }

    if (errno != 0)
        goto ERR;

    if (closedir(dirp) < 0)
        goto ERR;

    return alcove_ok(t);

BADARG:
    erl_free(name);
    erl_free(t);
    return erl_mk_atom("badarg");

ERR:
    errnum = errno;
    erl_free(name);
    erl_free(t);
    return alcove_errno(errnum);
}
