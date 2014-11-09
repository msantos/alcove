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
 * getgid(2)
 *
 */
    ssize_t
alcove_sys_getgid(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    return alcove_mk_ulong(reply, rlen, getgid());
}

/*
 * getuid(2)
 *
 */
    ssize_t
alcove_sys_getuid(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    return alcove_mk_ulong(reply, rlen, getuid());
}

/*
 * setgid(2)
 *
 */
    ssize_t
alcove_sys_setgid(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;
    gid_t gid = {0};
    int rv = 0;

    /* gid */
    if (alcove_decode_uint(arg, len, &index, &gid) < 0)
        return -1;

    rv = setgid(gid);

    return (rv < 0)
        ? alcove_mk_errno(reply, rlen, errno)
        : alcove_mk_atom(reply, rlen, "ok");
}

/*
 * setuid(2)
 *
 */
    ssize_t
alcove_sys_setuid(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;
    uid_t uid = {0};
    int rv = 0;

    /* uid */
    if (alcove_decode_uint(arg, len, &index, &uid) < 0)
        return -1;

    rv = setuid(uid);

    return (rv < 0)
        ? alcove_mk_errno(reply, rlen, errno)
        : alcove_mk_atom(reply, rlen, "ok");
}

/*
 * getresuid(2)
 */
    ssize_t
alcove_sys_getresuid(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
#if defined(__sunos__)
    return alcove_mk_error(reply, rlen, "unsupported");
#else
    int rindex = 0;
    uid_t uid = {0};
    uid_t euid = {0};
    uid_t suid = {0};
    int rv = 0;

    rv = getresuid(&uid, &euid, &suid);

    if (rv < 0)
        return  alcove_mk_errno(reply, rlen, errno);

    ALCOVE_ERR(alcove_encode_version(reply, rlen, &rindex));
    ALCOVE_ERR(alcove_encode_tuple_header(reply, rlen, &rindex, 4));
    ALCOVE_ERR(alcove_encode_atom(reply, rlen, &rindex, "ok"));
    ALCOVE_ERR(alcove_encode_ulonglong(reply, rlen, &rindex, uid));
    ALCOVE_ERR(alcove_encode_ulonglong(reply, rlen, &rindex, euid));
    ALCOVE_ERR(alcove_encode_ulonglong(reply, rlen, &rindex, suid));

    return rindex;
#endif
}

/*
 * setresuid(2)
 */
    ssize_t
alcove_sys_setresuid(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
#if defined(__sunos__)
    return alcove_mk_error(reply, rlen, "unsupported");
#else
    int index = 0;
    uid_t uid = {0};
    uid_t euid = {0};
    uid_t suid = {0};
    int rv = 0;

    /* uid */
    if (alcove_decode_uint(arg, len, &index, &uid) < 0)
        return -1;

    /* euid */
    if (alcove_decode_uint(arg, len, &index, &euid) < 0)
        return -1;

    /* suid */
    if (alcove_decode_uint(arg, len, &index, &suid) < 0)
        return -1;

    rv = setresuid(uid, euid, suid);

    return (rv < 0)
        ? alcove_mk_errno(reply, rlen, errno)
        : alcove_mk_atom(reply, rlen, "ok");
#endif
}

/*
 * getresgid(2)
 */
    ssize_t
alcove_sys_getresgid(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
#if defined(__sunos__)
    return alcove_mk_error(reply, rlen, "unsupported");
#else
    int rindex = 0;
    gid_t gid = {0};
    gid_t egid = {0};
    gid_t sgid = {0};
    int rv = 0;

    rv = getresgid(&gid, &egid, &sgid);

    if (rv < 0)
        return  alcove_mk_errno(reply, rlen, errno);

    ALCOVE_ERR(alcove_encode_version(reply, rlen, &rindex));
    ALCOVE_ERR(alcove_encode_tuple_header(reply, rlen, &rindex, 4));
    ALCOVE_ERR(alcove_encode_atom(reply, rlen, &rindex, "ok"));
    ALCOVE_ERR(alcove_encode_ulonglong(reply, rlen, &rindex, gid));
    ALCOVE_ERR(alcove_encode_ulonglong(reply, rlen, &rindex, egid));
    ALCOVE_ERR(alcove_encode_ulonglong(reply, rlen, &rindex, sgid));

    return rindex;
#endif
}

/*
 * setresgid(2)
 */
    ssize_t
alcove_sys_setresgid(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
#if defined(__sunos__)
    return alcove_mk_error(reply, rlen, "unsupported");
#else
    int index = 0;
    gid_t gid = {0};
    gid_t egid = {0};
    gid_t sgid = {0};
    int rv = 0;

    /* gid */
    if (alcove_decode_uint(arg, len, &index, &gid) < 0)
        return -1;

    /* egid */
    if (alcove_decode_uint(arg, len, &index, &egid) < 0)
        return -1;

    /* sgid */
    if (alcove_decode_uint(arg, len, &index, &sgid) < 0)
        return -1;

    rv = setresgid(gid, egid, sgid);

    return (rv < 0)
        ? alcove_mk_errno(reply, rlen, errno)
        : alcove_mk_atom(reply, rlen, "ok");
#endif
}
