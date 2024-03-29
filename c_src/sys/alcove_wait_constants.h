/* Copyright (c) 2016-2021, Michael Santos <michael.santos@gmail.com>
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
#include <sys/wait.h>

static const alcove_constant_t alcove_wait_constants[] = {
#ifdef WNOHANG
    ALCOVE_CONSTANT(WNOHANG),
#endif
#ifdef WUNTRACED
    ALCOVE_CONSTANT(WUNTRACED),
#endif
#ifdef WCONTINUED
    ALCOVE_CONSTANT(WCONTINUED),
#endif

#ifdef __FreeBSD__
    ALCOVE_CONSTANT(P_PID),
    ALCOVE_CONSTANT(P_PGID),
#endif

    {NULL, 0}};
