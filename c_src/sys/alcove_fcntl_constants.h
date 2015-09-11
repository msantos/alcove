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
static const alcove_define_t alcove_fcntl_constants[] = {
#ifdef F_GETFD
    ALCOVE_DEFINE(F_GETFD),
#endif
#ifdef F_SETFD
    ALCOVE_DEFINE(F_SETFD),
#endif
#ifdef FD_CLOEXEC
    ALCOVE_DEFINE(FD_CLOEXEC),
#endif
#ifdef F_SETPIPE_SZ
    ALCOVE_DEFINE(F_SETPIPE_SZ),
#endif
#ifdef F_GETPIPE_SZ
    ALCOVE_DEFINE(F_GETPIPE_SZ),
#endif
    {NULL, 0}
};
