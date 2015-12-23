/* Copyright (c) 2015, Michael Santos <michael.santos@gmail.com>
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
 * fexecve(2)
 *
 */
    ssize_t
alcove_sys_fexecve(alcove_state_t *ap, const char *arg, size_t len,
        char *reply, size_t rlen)
{
#if defined(__linux__) || defined(__FreeBSD__)
    int index = 0;

    int fd = -1;
    char **argv = NULL;
    char **envp = NULL;
    int errnum = 0;

    /* fd */
    if (alcove_decode_int(arg, len, &index, &fd) < 0)
        return -1;

    /* argv */
    if (alcove_decode_argv(arg, len, &index, &argv) < 0)
        return -1;

    /* envp */
    if (alcove_decode_argv(arg, len, &index, &envp) < 0)
        return -1;

    fexecve(fd, argv, envp);

    errnum = errno;

    alcove_free_argv(argv);
    alcove_free_argv(envp);

    return alcove_mk_errno(reply, rlen, errnum);
#else
    return alcove_mk_atom(reply, rlen, "undef");
#endif
}
