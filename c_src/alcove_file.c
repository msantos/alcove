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
#include <sys/select.h>

#include "alcove_file.h"

static int alcove_list_to_fd_set(const char *arg, int *index,
        fd_set *fdset, int *nfds);
static int alcove_fd_isset(char *buf, int *index, fd_set *set);

/*
 * open(2)
 *
 */
    ssize_t
alcove_open(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;
    int rindex = 0;
    char pathname[PATH_MAX] = {0};
    size_t plen = sizeof(pathname)-1;
    int flags = 0;
    mode_t mode = {0};
    int fd = 0;
    int errnum = 0;

    /* pathname */
    if (alcove_decode_iolist_to_binary(arg, &index, pathname, &plen) < 0 ||
            plen == 0)
        return -1;

    /* flags */
    if (alcove_decode_int(arg, &index, &flags) < 0)
        return -1;

    /* mode */
    if (alcove_decode_uint(arg, &index, (u_int32_t *)&mode) < 0)
        return -1;

    fd = open(pathname, flags, mode);

    if (fd < 0)
        goto ERR;

    flags = fcntl(fd, F_GETFD, 0);
    if (flags < 0)
        goto ERR;

    if (fcntl(fd, F_SETFD, flags | FD_CLOEXEC) < 0)
        goto ERR;

    ALCOVE_OK(
        reply,
        &rindex,
        ei_encode_long(reply, &rindex, fd)
    );

    return rindex;

ERR:
    errnum = errno;
    (void)close(fd);
    return alcove_errno(reply, rlen, errnum);
}

/*
 * close(2)
 *
 */
    ssize_t
alcove_close(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;
    int fd = 0;

    /* fd */
    if (alcove_decode_int(arg, &index, &fd) < 0)
        return -1;

    /* stdin, stdout, stderr, ctl are reserved */
    if (fd < 4)
        return alcove_errno(reply, rlen, EBADF);

    return (close(fd) < 0)
        ? alcove_errno(reply, rlen, errno)
        : alcove_mk_atom(reply, rlen, "ok");
}

/*
 * select(2)
 *
 */
    ssize_t
alcove_select(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;
    int rindex = 0;
    int type = 0;
    int arity = 0;

    int nfds = 0;
    fd_set readfds;
    fd_set writefds;
    fd_set exceptfds;

    char buf[MAXATOMLEN] = {0};
    struct timeval tv = {0};
    struct timeval *timeout = NULL;

    int rv = 0;

    /* readfds */
    if (alcove_list_to_fd_set(arg, &index, &readfds, &nfds) < 0)
        return alcove_errno(reply, rlen, EBADF);

    /* writefds */
    if (alcove_list_to_fd_set(arg, &index, &writefds, &nfds) < 0)
        return alcove_errno(reply, rlen, EBADF);

    /* exceptfds */
    if (alcove_list_to_fd_set(arg, &index, &exceptfds, &nfds) < 0)
        return alcove_errno(reply, rlen, EBADF);

    /* timeout */
    if (ei_get_type(arg, &index, &type, &arity) < 0)
        return -1;

    switch (type) {
        case ERL_BINARY_EXT:
            if (arity > 0)
                return -1;

            timeout = NULL;
            break;

        case ERL_SMALL_TUPLE_EXT:
        case ERL_LARGE_TUPLE_EXT:
            if (arity != 3)
                return -1;

            timeout = &tv;

            /* 'alcove_timeval' */
            if (ei_decode_atom(arg, &index, buf) < 0)
                return -1;

            if (strncmp(buf, "alcove_timeval", 14))
                return -1;

            /* sec */
            if (ei_decode_longlong(arg, &index, (long long *)&tv.tv_sec) < 0)
                return -1;

            /* usec */
            if (ei_decode_longlong(arg, &index, (long long *)&tv.tv_usec) < 0)
                return -1;

            break;

        default:
            return -1;
    }

    rv = select(nfds+1, &readfds, &writefds, &exceptfds, timeout);

    if (rv < 0)
        return alcove_errno(reply, rindex, errno);

    ALCOVE_TUPLE4(reply, &rindex,
        "ok",
        alcove_fd_isset(reply, &rindex, &readfds),
        alcove_fd_isset(reply, &rindex, &writefds),
        alcove_fd_isset(reply, &rindex, &exceptfds)
    );

    return rindex;
}

/*
 * lseek(2)
 *
 */
    ssize_t
alcove_lseek(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;

    int fd = 0;
    off_t offset = 0;
    int whence = 0;

    /* fd */
    if (alcove_decode_int(arg, &index, &fd) < 0)
        return -1;

    /* stdin, stdout, stderr, ctl are reserved */
    if (fd < 4)
        return alcove_errno(reply, rlen, EBADF);

    /* offset */
    if (ei_decode_longlong(arg, &index, (long long *)&offset) < 0)
        return -1;

    /* whence */
    if (alcove_decode_int(arg, &index, &whence) < 0)
        return -1;

    return (lseek(fd, offset, whence) == -1)
        ? alcove_errno(reply, rlen, errno)
        : alcove_mk_atom(reply, rlen, "ok");
}

/*
 * read(2)
 *
 */
    ssize_t
alcove_read(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;
    int rindex = 0;

    int fd = -1;
    size_t count = 0;
    char buf[MAXMSGLEN] = {0};
    size_t maxlen = 0;
    int rv = 0;

    /* fd */
    if (alcove_decode_int(arg, &index, &fd) < 0)
        return -1;

    /* stdin, stdout, stderr, ctl are reserved */
    if (fd < 4)
        return alcove_errno(reply, rlen, EBADF);

    /* count */
    if (ei_decode_ulonglong(arg, &index, (unsigned long long *)&count) < 0)
        return -1;

    /* Silently truncate too large values of count */
    maxlen = ALCOVE_MSGLEN(ap->depth, sizeof(buf));
    if (count > maxlen)
        count = maxlen;

    rv = read(fd, buf, count);

    if (rv < 0)
        return alcove_errno(reply, rlen, errno);

    ALCOVE_OK(reply, &rindex,
        ei_encode_binary(reply, &rindex, buf, rv));

    return rindex;
}

/*
 * write(2)
 *
 */
    ssize_t
alcove_write(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;
    int rindex = 0;

    int fd = -1;
    char buf[MAXMSGLEN] = {0};
    size_t buflen = sizeof(buf);
    int rv = 0;

    /* fd */
    if (alcove_decode_int(arg, &index, &fd) < 0)
        return -1;

    /* stdin, stdout, stderr, ctl are reserved */
    if (fd < 4)
        return alcove_errno(reply, rlen, EBADF);

    /* buf */
    if (alcove_decode_iolist_to_binary(arg, &index, buf, &buflen) < 0)
        return -1;

    rv = write(fd, buf, buflen);

    if (rv < 0) {
        rindex = alcove_errno(reply, rlen, errno);
    }
    else {
        ALCOVE_OK(reply, &rindex,
            ei_encode_longlong(reply, &rindex, rv));
    }

    return rindex;
}

/*
 * chmod(2)
 *
 */
    ssize_t
alcove_chmod(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;

    char path[PATH_MAX] = {0};
    size_t plen = sizeof(path)-1;
    mode_t mode = {0};
    int rv = 0;

    /* path */
    if (alcove_decode_iolist_to_binary(arg, &index, path, &plen) < 0 ||
            plen == 0)
        return -1;

    /* mode */
    if (alcove_decode_uint(arg, &index, (u_int32_t *)&mode) < 0)
        return -1;

    rv = chmod(path, mode);

    return (rv < 0)
        ? alcove_errno(reply, rlen, errno)
        : alcove_mk_atom(reply, rlen, "ok");
}

/*
 * chown(2)
 *
 */
    ssize_t
alcove_chown(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;

    char path[PATH_MAX] = {0};
    size_t plen = sizeof(path)-1;
    uid_t owner = {0};
    gid_t group = {0};
    int rv = 0;

    /* path */
    if (alcove_decode_iolist_to_binary(arg, &index, path, &plen) < 0 ||
            plen == 0)
        return -1;

    /* owner */
    if (alcove_decode_uint(arg, &index, &owner) < 0)
        return -1;

    /* group */
    if (alcove_decode_uint(arg, &index, &group) < 0)
        return -1;

    rv = chown(path, owner, group);

    return (rv < 0)
        ? alcove_errno(reply, rlen, errno)
        : alcove_mk_atom(reply, rlen, "ok");
}

/*
 * file flags
 *
 */
    ssize_t
alcove_file_define(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;
    int rindex = 0;

    char flag[MAXATOMLEN] = {0};

    /* flag */
    if (ei_decode_atom(arg, &index, flag) < 0)
        return -1;

    ALCOVE_ERR(ei_encode_version(reply, &rindex));
    ALCOVE_ERR(alcove_define(reply, &rindex, flag, alcove_file_constants));
    return rindex;
}

/*
 * Utility functions
 *
 */
    static int
alcove_list_to_fd_set(const char *arg, int *index, fd_set *fdset, int *nfds)
{
    int type = 0;
    int arity = 0;

    int i = 0;
    int fd = -1;

    if (ei_get_type(arg, index, &type, &arity) < 0)
        return -1;

    if (arity >= FD_SETSIZE)
        return -1;

    FD_ZERO(fdset);

    switch (type) {
        case ERL_STRING_EXT: {
            char tmp[FD_SETSIZE] = {0};

            if (ei_decode_string(arg, index, tmp) < 0)
                return -1;

            for (i = 0; i < arity; i++) {
                fd = tmp[i];

                if (fd < 4 || fd >= FD_SETSIZE || fcntl(fd, F_GETFD, 0) < 0)
                    return -1;

                FD_SET(fd, fdset);
                *nfds = MAX(fd, *nfds);
            }
            }
            break;

        case ERL_LIST_EXT:
            if (ei_decode_list_header(arg, index, &arity) < 0)
                return -1;

            for (i = 0; i < arity; i++) {
                if (alcove_decode_int(arg, index, &fd) < 0)
                    return -1;

                /* stdin, stdout, stderr, ctl are reserved */
                if (fd < 4 || fd >= FD_SETSIZE || fcntl(fd, F_GETFD, 0) < 0)
                    return -1;

                FD_SET(fd, fdset);
                *nfds = MAX(fd, *nfds);
            }

            if (ei_decode_list_header(arg, index, &arity) < 0 || arity != 0)
                return -1;

            break;

        case ERL_NIL_EXT:
            if (ei_decode_list_header(arg, index, &arity) < 0 || arity != 0)
                return -1;

            break;

        default:
            return -1;
    }

    return 0;
}

    static int
alcove_fd_isset(char *buf, int *index, fd_set *set)
{
    int fd = 0;

    if (!set)
        return 0;

    for (fd = FD_SETSIZE - 1; fd > 3; fd--) {
        if (FD_ISSET(fd, set)) {
            if (ei_encode_list_header(buf, index, 1) < 0)
                return -1;

            if (ei_encode_long(buf, index, fd) < 0)
                return -1;
        }
    }

    if (ei_encode_empty_list(buf, index) < 0)
        return -1;

    return 0;
}
