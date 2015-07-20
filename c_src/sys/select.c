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
#include <sys/select.h>

static int alcove_list_to_fd_set(const char *arg, size_t len, int *index,
        fd_set *fdset, int *nfds);
static int alcove_fd_isset(char *buf, size_t len, int *index, fd_set *set);

/*
 * select(2)
 *
 */
    ssize_t
alcove_sys_select(alcove_state_t *ap, const char *arg, size_t len,
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
    if (alcove_list_to_fd_set(arg, len, &index, &readfds, &nfds) < 0)
        return alcove_mk_errno(reply, rlen, EBADF);

    /* writefds */
    if (alcove_list_to_fd_set(arg, len, &index, &writefds, &nfds) < 0)
        return alcove_mk_errno(reply, rlen, EBADF);

    /* exceptfds */
    if (alcove_list_to_fd_set(arg, len, &index, &exceptfds, &nfds) < 0)
        return alcove_mk_errno(reply, rlen, EBADF);

    /* timeout */
    if (alcove_get_type(arg, len, &index, &type, &arity) < 0)
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
            if (alcove_decode_atom(arg, len, &index, buf) < 0)
                return -1;

            if (strcmp(buf, "alcove_timeval") != 0)
                return -1;

            /* sec */
            if (alcove_decode_longlong(arg, len, &index, (long long *)&tv.tv_sec) < 0)
                return -1;

            /* usec */
            if (alcove_decode_longlong(arg, len, &index, (long long *)&tv.tv_usec) < 0)
                return -1;

            break;

        default:
            return -1;
    }

    rv = select(nfds+1, &readfds, &writefds, &exceptfds, timeout);

    if (rv < 0)
        return alcove_mk_errno(reply, rindex, errno);

    ALCOVE_TUPLE4(reply, &rindex,
        "ok",
        alcove_fd_isset(reply, rlen, &rindex, &readfds),
        alcove_fd_isset(reply, rlen, &rindex, &writefds),
        alcove_fd_isset(reply, rlen, &rindex, &exceptfds)
    );

    return rindex;
}

    static int
alcove_list_to_fd_set(const char *arg, size_t len, int *index,
        fd_set *fdset, int *nfds)
{
    int type = 0;
    int arity = 0;

    int i = 0;
    int fd = -1;

    if (alcove_get_type(arg, len, index, &type, &arity) < 0)
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

                if (fd >= FD_SETSIZE || fcntl(fd, F_GETFD, 0) < 0)
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
                if (alcove_decode_int(arg, len, index, &fd) < 0)
                    return -1;

                if (fd >= FD_SETSIZE || fcntl(fd, F_GETFD, 0) < 0)
                    return -1;

                FD_SET(fd, fdset);
                *nfds = MAX(fd, *nfds);
            }

            if (alcove_decode_list_header(arg, len, index, &arity) < 0
                    || arity != 0)
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
alcove_fd_isset(char *buf, size_t len, int *index, fd_set *set)
{
    int fd = 0;

    if (set == NULL)
        return 0;

    for (fd = FD_SETSIZE - 1; fd > 3; fd--) {
        if (FD_ISSET(fd, set)) {
            if (alcove_encode_list_header(buf, len, index, 1) < 0)
                return -1;

            if (alcove_encode_long(buf, len, index, fd) < 0)
                return -1;
        }
    }

    if (alcove_encode_empty_list(buf, len, index) < 0)
        return -1;

    return 0;
}
