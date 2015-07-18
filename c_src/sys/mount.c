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
#include "alcove_mount_constants.h"

#define MAYBE_NULL(_len, _buf) ((_len) == 0 ? NULL : (_buf))


/*
 * mount(2)
 *
 */
    ssize_t
alcove_sys_mount(alcove_state_t *ap, const char *arg, size_t len,
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
    char opt[MAXMSGLEN] = {0};
    size_t olen = sizeof(opt);

    int rv = 0;

    /* source */
    if (alcove_decode_iolist(arg, len, &index, source, &slen) < 0)
        return -1;

    /* target */
    if (alcove_decode_iolist(arg, len, &index, target, &tlen) < 0 ||
            tlen == 0)
        return -1;

    /* filesystemtype */
    if (alcove_decode_iolist(arg, len, &index, filesystemtype, &flen) < 0)
        return -1;

    /* mountflags */
    switch (alcove_decode_define_list(arg, len, &index, (int *)&mountflags,
                alcove_mount_constants)) {
        case 0:
            break;
        case 1:
            return alcove_mk_error(reply, rlen, "unsupported");
        default:
            return -1;
    }

    /* data */
    if (alcove_decode_iolist(arg, len, &index, data, &dlen) < 0)
        return -1;

    /* options */
    if (alcove_decode_iolist(arg, len, &index, opt, &olen) < 0)
        return -1;

#ifdef __linux__
    rv = mount(
            MAYBE_NULL(slen, source),
            target,
            MAYBE_NULL(flen, filesystemtype),
            mountflags,
            MAYBE_NULL(dlen, data)
            );
#elif defined(__sunos__)
    /* The option buffer is input/output with the mount options placed
     * in the buf on return. The option buffer must be large enough to
     * hold the returned options string.
     *
     * * if the buffer is too small, the return value is {error,eoverflow}
     *   and the mount is successful
     *
     * * if the buffer is too large(?), the return value is {error,einval}
     *   and the mount fails
     */
    rv = mount(
            MAYBE_NULL(slen, source),
            target,
            mountflags,
            MAYBE_NULL(flen, filesystemtype),
            MAYBE_NULL(dlen, data),
            dlen,
            MAYBE_NULL(olen, opt),
            olen
            );
#else
    rv = mount(
            MAYBE_NULL(flen, filesystemtype),
            target,
            mountflags,
            MAYBE_NULL(dlen, data)
#ifdef __NetBSD__
            , dlen
#endif
            );
#endif

    return (rv < 0)
        ? alcove_mk_errno(reply, rlen, errno)
        : alcove_mk_atom(reply, rlen, "ok");
}
