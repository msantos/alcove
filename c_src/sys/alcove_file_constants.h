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
static const alcove_constant_t alcove_file_constants[] = {
#ifdef O_ACCMODE
    ALCOVE_CONSTANT(O_ACCMODE),
#endif
#ifdef O_RDONLY
    ALCOVE_CONSTANT(O_RDONLY),
#endif
#ifdef O_WRONLY
    ALCOVE_CONSTANT(O_WRONLY),
#endif
#ifdef O_RDWR
    ALCOVE_CONSTANT(O_RDWR),
#endif
#ifdef O_CREAT
    ALCOVE_CONSTANT(O_CREAT),
#endif
#ifdef O_EXCL
    ALCOVE_CONSTANT(O_EXCL),
#endif
#ifdef O_NOCTTY
    ALCOVE_CONSTANT(O_NOCTTY),
#endif
#ifdef O_TRUNC
    ALCOVE_CONSTANT(O_TRUNC),
#endif
#ifdef O_APPEND
    ALCOVE_CONSTANT(O_APPEND),
#endif
#ifdef O_NONBLOCK
    ALCOVE_CONSTANT(O_NONBLOCK),
#endif
#ifdef O_DSYNC
    ALCOVE_CONSTANT(O_DSYNC),
#endif
#ifdef O_DIRECT
    ALCOVE_CONSTANT(O_DIRECT),
#endif
#ifdef O_LARGEFILE
    ALCOVE_CONSTANT(O_LARGEFILE),
#endif
#ifdef O_DIRECTORY
    ALCOVE_CONSTANT(O_DIRECTORY),
#endif
#ifdef O_NOFOLLOW
    ALCOVE_CONSTANT(O_NOFOLLOW),
#endif
#ifdef O_NOATIME
    ALCOVE_CONSTANT(O_NOATIME),
#endif
#ifdef O_CLOEXEC
    ALCOVE_CONSTANT(O_CLOEXEC),
#endif
#ifdef __O_SYNC
    ALCOVE_CONSTANT(__O_SYNC),
#endif
#ifdef O_SYNC
    ALCOVE_CONSTANT(O_SYNC),
#endif
#ifdef O_PATH
    ALCOVE_CONSTANT(O_PATH),
#endif
#ifdef O_NDELAY
    ALCOVE_CONSTANT(O_NDELAY),
#endif
    {NULL, 0}
};
