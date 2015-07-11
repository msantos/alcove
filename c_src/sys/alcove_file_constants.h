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
static const alcove_define_t alcove_file_constants[] = {
#ifdef O_ACCMODE
    ALCOVE_DEFINE(O_ACCMODE),
#endif
#ifdef O_RDONLY
    ALCOVE_DEFINE(O_RDONLY),
#endif
#ifdef O_WRONLY
    ALCOVE_DEFINE(O_WRONLY),
#endif
#ifdef O_RDWR
    ALCOVE_DEFINE(O_RDWR),
#endif
#ifdef O_CREAT
    ALCOVE_DEFINE(O_CREAT),
#endif
#ifdef O_EXCL
    ALCOVE_DEFINE(O_EXCL),
#endif
#ifdef O_NOCTTY
    ALCOVE_DEFINE(O_NOCTTY),
#endif
#ifdef O_TRUNC
    ALCOVE_DEFINE(O_TRUNC),
#endif
#ifdef O_APPEND
    ALCOVE_DEFINE(O_APPEND),
#endif
#ifdef O_NONBLOCK
    ALCOVE_DEFINE(O_NONBLOCK),
#endif
#ifdef O_DSYNC
    ALCOVE_DEFINE(O_DSYNC),
#endif
#ifdef O_DIRECT
    ALCOVE_DEFINE(O_DIRECT),
#endif
#ifdef O_LARGEFILE
    ALCOVE_DEFINE(O_LARGEFILE),
#endif
#ifdef O_DIRECTORY
    ALCOVE_DEFINE(O_DIRECTORY),
#endif
#ifdef O_NOFOLLOW
    ALCOVE_DEFINE(O_NOFOLLOW),
#endif
#ifdef O_NOATIME
    ALCOVE_DEFINE(O_NOATIME),
#endif
#ifdef O_CLOEXEC
    ALCOVE_DEFINE(O_CLOEXEC),
#endif
#ifdef __O_SYNC
    ALCOVE_DEFINE(__O_SYNC),
#endif
#ifdef O_SYNC
    ALCOVE_DEFINE(O_SYNC),
#endif
#ifdef O_PATH
    ALCOVE_DEFINE(O_PATH),
#endif
#ifdef O_NDELAY
    ALCOVE_DEFINE(O_NDELAY),
#endif
    {NULL, 0}
};
