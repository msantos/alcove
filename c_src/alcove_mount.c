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
    char *data = NULL;

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
        data = erl_iolist_to_string(hd);

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
    char *flag = NULL;

    /* flag */
    arg = alcove_list_head(&hd, arg);
    if (!hd || !ERL_IS_ATOM(hd))
        goto BADARG;

    flag = ERL_ATOM_PTR(hd);

#ifdef MS_RDONLY
    if      (!strncmp(flag, "rdonly", 6))           return erl_mk_int(MS_RDONLY);
#elif MNT_RDONLY
    if      (!strncmp(flag, "rdonly", 6))           return erl_mk_int(MNT_RDONLY);
#endif
#ifdef MS_NOSUID
    else if (!strncmp(flag, "nosuid", 6))           return erl_mk_int(MS_NOSUID);
#elif MNT_NOSUID
    else if (!strncmp(flag, "nosuid", 6))           return erl_mk_int(MNT_NOSUID);
#endif
#ifdef MS_NODEV
    else if (!strncmp(flag, "nodev", 5))            return erl_mk_int(MS_NODEV);
#endif
#ifdef MS_NOEXEC
    else if (!strncmp(flag, "noexec", 6))           return erl_mk_int(MS_NOEXEC);
#elif MNT_NOEXEC
    else if (!strncmp(flag, "noexec", 6))           return erl_mk_int(MNT_NOEXEC);
#endif
#ifdef MS_SYNCHRONOUS
    else if (!strncmp(flag, "synchronous", 11))     return erl_mk_int(MS_SYNCHRONOUS);
#endif
#ifdef MS_REMOUNT
    else if (!strncmp(flag, "remount", 7))          return erl_mk_int(MS_REMOUNT);
#endif
#ifdef MS_MANDLOCK
    else if (!strncmp(flag, "mandlock", 8))         return erl_mk_int(MS_MANDLOCK);
#endif
#ifdef MS_DIRSYNC
    else if (!strncmp(flag, "dirsync", 7))          return erl_mk_int(MS_DIRSYNC);
#endif
#ifdef MS_NOATIME
    else if (!strncmp(flag, "noatime", 7))          return erl_mk_int(MS_NOATIME);
#endif
#ifdef MS_NODIRATIME
    else if (!strncmp(flag, "nodiratime", 10))      return erl_mk_int(MS_NODIRATIME);
#endif
#ifdef MS_BIND
    else if (!strncmp(flag, "bind", 4))             return erl_mk_int(MS_BIND);
#endif
#ifdef MS_MOVE
    else if (!strncmp(flag, "move", 4))             return erl_mk_int(MS_MOVE);
#endif
#ifdef MS_REC
    else if (!strncmp(flag, "rec", 3))              return erl_mk_int(MS_REC);
#endif
#ifdef MS_SILENT
    else if (!strncmp(flag, "silent", 6))           return erl_mk_int(MS_SILENT);
#endif
#ifdef MS_POSIXACL
    else if (!strncmp(flag, "posixacl", 8))         return erl_mk_int(MS_POSIXACL);
#endif
#ifdef MS_UNBINDABLE
    else if (!strncmp(flag, "unbindable", 10))      return erl_mk_int(MS_UNBINDABLE);
#endif
#ifdef MS_PRIVATE
    else if (!strncmp(flag, "private", 7))          return erl_mk_int(MS_PRIVATE);
#endif
#ifdef MS_SLAVE
    else if (!strncmp(flag, "slave", 5))            return erl_mk_int(MS_SLAVE);
#endif
#ifdef MS_SHARED
    else if (!strncmp(flag, "shared", 6))           return erl_mk_int(MS_SHARED);
#endif
#ifdef MS_REALTIME
    else if (!strncmp(flag, "realtime", 8))         return erl_mk_int(MS_REALTIME);
#endif
#ifdef MS_RELATIME
    else if (!strncmp(flag, "relatime", 8))         return erl_mk_int(MS_RELATIME);
#endif
#ifdef MS_KERNMOUNT
    else if (!strncmp(flag, "kernmount", 9))        return erl_mk_int(MS_KERNMOUNT);
#endif
#ifdef MS_I_VERSION
    else if (!strncmp(flag, "i_version", 9))        return erl_mk_int(MS_I_VERSION);
#endif
#ifdef MS_STRICTATIME
    else if (!strncmp(flag, "strictatime", 11))     return erl_mk_int(MS_STRICTATIME);
#endif
#ifdef MS_ACTIVE
    else if (!strncmp(flag, "active", 6))           return erl_mk_int(MS_ACTIVE);
#endif
#ifdef MS_NOUSER
    else if (!strncmp(flag, "nouser", 6))           return erl_mk_int(MS_NOUSER);
#endif
    else return erl_mk_atom("false");

BADARG:
    return erl_mk_atom("badarg");
}
