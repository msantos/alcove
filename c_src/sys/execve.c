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
 * execve(2)
 *
 */
    ssize_t
alcove_sys_execve(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
    int index = 0;

    char filename[PATH_MAX] = {0};
    size_t flen = sizeof(filename)-1;
    char **argv = NULL;
    char **envp = NULL;
    int errnum = 0;

    /* filename */
    if (alcove_decode_iolist(arg, len, &index, filename, &flen) ||
            flen == 0)
        return -1;

    /* argv */
    if (alcove_decode_list_to_argv(arg, len, &index, &argv) < 0)
        return -1;

    /* envp */
    if (alcove_decode_list_to_argv(arg, len, &index, &envp) < 0)
        return -1;

    execve(filename, argv, envp);

    errnum = errno;

    alcove_free_argv(argv);
    alcove_free_argv(envp);

    return alcove_mk_errno(reply, rlen, errnum);
}
