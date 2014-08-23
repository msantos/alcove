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
    ssize_t
alcove_mount(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;

    char source[PATH_MAX] = {0};
    size_t slen = sizeof(source)-1;
    char target[PATH_MAX] = {0};
    size_t tlen = sizeof(target)-1;
    char filesystemtype[PATH_MAX] = {0};
    size_t flen = sizeof(filesystemtype)-1;
    unsigned long mountflags = 0;
    char data[MAXMSGLEN] = {0};
    size_t dlen = sizeof(data);

    int rv = 0;

    /* source */
    if (alcove_decode_iolist(arg, &index, source, &slen) < 0)
        return -1;

    /* target */
    if (alcove_decode_iolist(arg, &index, target, &tlen) < 0 ||
            tlen == 0)
        return -1;

    /* filesystemtype */
    if (alcove_decode_iolist(arg, &index, filesystemtype, &flen) < 0)
        return -1;

    /* mountflags */
    if (ei_decode_ulong(arg, &index, &mountflags) < 0)
        return -1;

    /* data */
    if (alcove_decode_iolist(arg, &index, data, &dlen) < 0)
        return -1;

#ifdef __linux__
    rv = mount(
            (slen == 0 ? NULL : source),
            target,
            (flen == 0 ? NULL : filesystemtype),
            mountflags,
            (dlen == 0 ? NULL : data)
            );
#else
    rv = mount(
            (flen == 0 ? NULL : filesystemtype),
            target,
            mountflags,
            (dlen == 0 ? NULL : data)
            );
#endif

    return (rv < 0)
        ? alcove_errno(reply, rlen, errno)
        : alcove_mk_atom(reply, rlen, "ok");
}

/*
 * umount(2)
 *
 */
    ssize_t
alcove_umount(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;

    char source[PATH_MAX] = {0};
    size_t slen = sizeof(source)-1;

    int rv = 0;

    /* source */
    if (alcove_decode_iolist(arg, &index, source, &slen) < 0 ||
            slen == 0)
        return -1;

#ifdef __linux__
    rv = umount(source);
#else
    rv = unmount(source, 0);
#endif

    return (rv < 0)
        ? alcove_errno(reply, rlen, errno)
        : alcove_mk_atom(reply, rlen, "ok");
}

/*
 * mount constants
 *
 */
    ssize_t
alcove_mount_define(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;
    int rindex = 0;

    char name[MAXATOMLEN] = {0};

    /* flag */
    if (alcove_decode_atom(arg, len, &index, name) < 0)
        return -1;

    ALCOVE_ERR(alcove_encode_version(reply, rlen, &rindex));
    ALCOVE_ERR(alcove_define(reply, rlen, &rindex,
                name, alcove_mount_constants));
    return rindex;
}
