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

    if (dirp == NULL)
        return alcove_mk_errno(reply, rlen, errno);

    ALCOVE_ERR(alcove_encode_version(reply, rlen, &rindex));
    ALCOVE_ERR(alcove_encode_tuple_header(reply, rlen, &rindex, 2));
    ALCOVE_ERR(alcove_encode_atom(reply, rlen, &rindex, "ok"));

    errno = 0;
    while ( (dent = readdir(dirp))) {
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
