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

#ifdef __linux__
#include <linux/unistd.h>
#endif

/*
 * chdir(2)
 *
 */
    ssize_t
alcove_sys_chdir(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;

    char path[PATH_MAX] = {0};
    size_t pathlen = sizeof(path)-1;
    int rv = 0;

    /* path */
    if (alcove_decode_iolist(arg, len, &index, path, &pathlen) < 0 ||
            pathlen == 0)
        return -1;

    rv = chdir(path);

    return (rv < 0)
        ? alcove_mk_errno(reply, rlen, errno)
        : alcove_mk_atom(reply, rlen, "ok");
}

/*
 * mkdir(2)
 *
 */
    ssize_t
alcove_sys_mkdir(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;
    char pathname[PATH_MAX] = {0};
    size_t pathlen = sizeof(pathname)-1;
    mode_t mode = {0};
    int rv = 0;

    /* pathname */
    if (alcove_decode_iolist(arg, len, &index, pathname, &pathlen) < 0 ||
            pathlen == 0)
        return -1;

    /* mode */
    if (alcove_decode_uint(arg, len, &index, (u_int32_t *)&mode) < 0)
        return -1;

    rv = mkdir(pathname, mode);

    return (rv < 0)
        ? alcove_mk_errno(reply, rlen, errno)
        : alcove_mk_atom(reply, rlen, "ok");
}

/*
 * rmdir(2)
 *
 */
    ssize_t
alcove_sys_rmdir(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;
    char pathname[PATH_MAX] = {0};
    size_t pathlen = sizeof(pathname)-1;
    int rv = 0;

    /* pathname */
    if (alcove_decode_iolist(arg, len, &index, pathname, &pathlen) < 0 ||
            pathlen == 0)
        return -1;

    rv = rmdir(pathname);

    return (rv < 0)
        ? alcove_mk_errno(reply, rlen, errno)
        : alcove_mk_atom(reply, rlen, "ok");
}

/*
 * chroot(2)
 *
 */
    ssize_t
alcove_sys_chroot(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;
    char path[PATH_MAX] = {0};
    size_t pathlen = sizeof(path)-1;
    int rv = 0;

    /* path */
    if (alcove_decode_iolist(arg, len, &index, path, &pathlen) < 0 ||
            pathlen == 0)
        return -1;

    rv = chroot(path);

    return (rv < 0)
        ? alcove_mk_errno(reply, rlen, errno)
        : alcove_mk_atom(reply, rlen, "ok");
}

/*
 * pivot_root(2)
 *
 */
    ssize_t
alcove_sys_pivot_root(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
#ifdef __linux__
    int index = 0;
    char new_root[PATH_MAX] = {0};
    char put_old[PATH_MAX] = {0};
    size_t nlen = sizeof(new_root)-1;
    size_t plen = sizeof(put_old)-1;
    int rv = 0;

    /* new_root */
    if (alcove_decode_iolist(arg, len, &index, new_root, &nlen) < 0 ||
            nlen == 0)
        return -1;

    /* put_old */
    if (alcove_decode_iolist(arg, len, &index, put_old, &plen) < 0 ||
            plen == 0)
        return -1;

    rv = syscall(__NR_pivot_root, new_root, put_old);

    return (rv < 0)
        ? alcove_mk_errno(reply, rlen, errno)
        : alcove_mk_atom(reply, rlen, "ok");
#else
    return alcove_mk_error(reply, rlen, "unsupported");
#endif
}

/*
 * getcwd(3)
 *
 */
    ssize_t
alcove_sys_getcwd(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int rindex = 0;
    char buf[PATH_MAX] = {0};

    if (!getcwd(buf, sizeof(buf)))
        return alcove_mk_errno(reply, rlen, errno);

    ALCOVE_OK(reply, &rindex,
            alcove_encode_binary(reply, rlen, &rindex, buf, strlen(buf)));

    return rindex;
}

/*
 * readdir(3)
 *
 */
    ssize_t
alcove_sys_readdir(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;
    int rindex = 0;

    char name[PATH_MAX] = {0};
    size_t namelen = sizeof(name)-1;
    DIR *dirp = NULL;
    struct dirent *dent = NULL;

    /* name */
    if (alcove_decode_iolist(arg, len, &index, name, &namelen) < 0 ||
            namelen == 0)
        return -1;

    dirp = opendir(name);

    if (!dirp)
        return alcove_mk_errno(reply, rlen, errno);

    ALCOVE_ERR(alcove_encode_version(reply, rlen, &rindex));
    ALCOVE_ERR(alcove_encode_tuple_header(reply, rlen, &rindex, 2));
    ALCOVE_ERR(alcove_encode_atom(reply, rlen, &rindex, "ok"));

    errno = 0;
    while ( (dent = readdir(dirp))) {
        if (!strcmp(dent->d_name, ".") || !strcmp(dent->d_name, ".."))
            continue;

        ALCOVE_ERR(alcove_encode_list_header(reply, rlen, &rindex, 1));

        ALCOVE_ERR(alcove_encode_binary(reply, rlen, &rindex,
                    dent->d_name, strlen(dent->d_name)));
    }

    if (errno != 0)
        return alcove_mk_errno(reply, rlen, errno);

    if (closedir(dirp) < 0)
        return alcove_mk_errno(reply, rlen, errno);

    ALCOVE_ERR(alcove_encode_empty_list(reply, rlen, &rindex));

    return rindex;
}
