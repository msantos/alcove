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
#include <fcntl.h>

#include "alcove_file.h"

/*
 * open(2)
 *
 */
    ETERM *
alcove_open(alcove_state_t *ap, ETERM *arg)
{
    ETERM *hd = NULL;
    char *pathname = NULL;
    int flags = 0;
    mode_t mode = {0};
    int fd = 0;
    int errnum = 0;

    /* pathname */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ALCOVE_IS_IOLIST(hd))
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        pathname = erl_iolist_to_string(hd);

    if (!pathname)
        goto BADARG;

    /* flags */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_INTEGER(hd))
        goto BADARG;

    flags = ERL_INT_VALUE(hd);

    /* mode */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ALCOVE_IS_UNSIGNED_INTEGER(hd))
        goto BADARG;

    mode = ERL_INT_UVALUE(hd);

    fd = open(pathname, flags, mode);

    if (fd < 0)
        goto ERR;

    flags = fcntl(fd, F_GETFD, 0);
    if (flags < 0)
        goto ERR;

    if (fcntl(fd, F_SETFD, flags | FD_CLOEXEC) < 0)
        goto ERR;

    erl_free(pathname);

    return alcove_ok(erl_mk_int(fd));

BADARG:
    erl_free(pathname);
    return erl_mk_atom("badarg");

ERR:
    errnum = errno;
    (void)close(fd);
    erl_free(pathname);
    return alcove_errno(errnum);
}

/*
 * close(2)
 *
 */
    ETERM *
alcove_close(alcove_state_t *ap, ETERM *arg)
{
    ETERM *hd = NULL;
    int fd = 0;

    /* fd */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_INTEGER(hd))
        goto BADARG;

    fd = ERL_INT_VALUE(hd);

    /* stdin, stdout, stderr, ctl are reserved */
    if (fd < 4)
        return alcove_errno(EBADF);

    return (close(fd) < 0)
        ? alcove_errno(errno)
        : erl_mk_atom("ok");

BADARG:
    return erl_mk_atom("badarg");
}

/*
 * lseek(2)
 *
 */
    ETERM *
alcove_lseek(alcove_state_t *ap, ETERM *arg)
{
    ETERM *hd = NULL;
    int fd = 0;
    off_t offset = 0;
    int whence = 0;

    /* fd */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_INTEGER(hd))
        goto BADARG;

    fd = ERL_INT_VALUE(hd);

    /* offset */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ALCOVE_IS_LONGLONG(hd))
        goto BADARG;

    offset = ALCOVE_LL_VALUE(hd);

    /* whence */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_INTEGER(hd))
        goto BADARG;

    offset = ERL_INT_VALUE(hd);

    /* stdin, stdout, stderr, ctl are reserved */
    if (fd < 4)
        return alcove_errno(EBADF);

    return (lseek(fd, offset, whence) == -1)
        ? alcove_errno(errno)
        : erl_mk_atom("ok");

BADARG:
    return erl_mk_atom("badarg");
}

/*
 * read(2)
 *
 */
    ETERM *
alcove_read(alcove_state_t *ap, ETERM *arg)
{
    ETERM *hd = NULL;
    int fd = -1;
    size_t count = 0;
    char buf[MAXMSGLEN] = {0};
    size_t maxlen = 0;
    int rv = 0;

    /* fd */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_INTEGER(hd))
        goto BADARG;

    fd = ERL_INT_VALUE(hd);

    /* stdin, stdout, stderr, ctl are reserved */
    if (fd < 4)
        return alcove_errno(EBADF);

    /* count */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ALCOVE_IS_UNSIGNED_LONGLONG(hd))
        goto BADARG;

    count = ALCOVE_LL_UVALUE(hd);

    /* Silently truncate too large values of count */
    maxlen = ALCOVE_MSGLEN(ap->depth, sizeof(buf));
    if (count > maxlen)
        count = maxlen;

    rv = read(fd, buf, count);

    if (rv < 0)
        return alcove_errno(errno);

    return alcove_ok(erl_mk_binary(buf, rv));

BADARG:
    return erl_mk_atom("badarg");
}

/*
 * write(2)
 *
 */
    ETERM *
alcove_write(alcove_state_t *ap, ETERM *arg)
{
    ETERM *hd = NULL;
    int fd = -1;
    ETERM *buf = NULL;
    int rv = 0;
    int errnum = 0;

    /* fd */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_INTEGER(hd))
        goto BADARG;

    fd = ERL_INT_VALUE(hd);

    /* stdin, stdout, stderr, ctl are reserved */
    if (fd < 4)
        return alcove_errno(EBADF);

    /* buf */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ALCOVE_IS_IOLIST(hd))
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        buf = erl_iolist_to_binary(hd);

    if (!buf)
        goto BADARG;

    rv = write(fd, ERL_BIN_PTR(buf), ERL_BIN_SIZE(buf));

    errnum = errno;

    erl_free(buf);

    return (rv < 0)
        ? alcove_errno(errnum)
        : alcove_ok(erl_mk_longlong(rv));

BADARG:
    erl_free(buf);
    return erl_mk_atom("badarg");
}

/*
 * chmod(2)
 *
 */
    ETERM *
alcove_chmod(alcove_state_t *ap, ETERM *arg)
{
    ETERM *hd = NULL;
    char *path = NULL;
    mode_t mode = {0};
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

    /* mode */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ALCOVE_IS_UNSIGNED_INTEGER(hd))
        goto BADARG;

    mode = ERL_INT_UVALUE(hd);

    rv = chmod(path, mode);

    errnum = errno;

    erl_free(path);

    return (rv < 0) ? alcove_errno(errnum) : erl_mk_atom("ok");

BADARG:
    return erl_mk_atom("badarg");
}

/*
 * chown(2)
 *
 */
    ETERM *
alcove_chown(alcove_state_t *ap, ETERM *arg)
{
    ETERM *hd = NULL;
    char *path = NULL;
    uid_t owner = {0};
    gid_t group = {0};
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

    /* owner */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ALCOVE_IS_UNSIGNED_INTEGER(hd))
        goto BADARG;

    owner = ERL_INT_UVALUE(hd);

    /* group */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ALCOVE_IS_UNSIGNED_INTEGER(hd))
        goto BADARG;

    group = ERL_INT_UVALUE(hd);

    rv = chown(path, owner, group);

    errnum = errno;

    erl_free(path);

    return (rv < 0) ? alcove_errno(errnum) : erl_mk_atom("ok");

BADARG:
    return erl_mk_atom("badarg");
}

/*
 * file flags
 *
 */
    ETERM *
alcove_file_define(alcove_state_t *ap, ETERM *arg)
{
    ETERM *hd = NULL;
    char *flag = NULL;

    /* flag */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_ATOM(hd))
        goto BADARG;

    flag = ERL_ATOM_PTR(hd);

    return alcove_define(flag, alcove_file_constants);

BADARG:
    return erl_mk_atom("badarg");
}
