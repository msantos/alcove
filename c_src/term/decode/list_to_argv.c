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

static char *alcove_x_decode_iolist_to_string(const char *buf, size_t len,
        int *index);
void alcove_free_argv(char **argv);

    int
alcove_decode_list_to_argv(const char *arg, size_t len, int *index,
        char ***argv)
{
    int arity = 0;
    int empty = 0;

    int i = 0;
    long maxarg = sysconf(_SC_ARG_MAX);

    if (alcove_decode_list_header(arg, len, index, &arity) < 0)
        return -1;

    if (arity < 0 || arity >= maxarg)
        return -1;

    /* NULL terminate */
    *argv = calloc(arity + 1, sizeof(char *));

    if (*argv == NULL)
        err(errno, "calloc");

    for (i = 0; i < arity; i++) {
        (*argv)[i] = alcove_x_decode_iolist_to_string(arg, len, index);
        if (!(*argv)[i])
            goto BADARG;
    }

    /* list tail */
    if (arity > 0 && (alcove_decode_list_header(arg, len, index, &empty) < 0
                || empty != 0))
        goto BADARG;

    return 0;

BADARG:
    alcove_free_argv(*argv);
    return -1;
}

    static char *
alcove_x_decode_iolist_to_string(const char *buf, size_t len, int *index)
{
    char tmp[MAXMSGLEN] = {0};
    size_t tmplen = sizeof(tmp) - 1;
    char *res = NULL;

    if (alcove_decode_iolist(buf, len, index, tmp, &tmplen) < 0)
        return NULL;

    res = strdup(tmp);
    if (res == NULL)
        err(errno, "strdup");

    return res;
}

    void
alcove_free_argv(char **argv)
{
    int i = 0;

    if (argv == NULL)
        return;

    for (i = 0; argv[i]; i++) {
        free(argv[i]);
        argv[i] = NULL;
    }

    free(argv);
    argv = NULL;
}
