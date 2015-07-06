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
#ifdef __linux__
#define _GNU_SOURCE
#include <sched.h>
#endif

static const alcove_define_t alcove_clone_constants[] = {
#ifdef CLONE_NEWNS
    ALCOVE_DEFINE(CLONE_NEWNS),
#endif
#ifdef CLONE_NEWUTS
    ALCOVE_DEFINE(CLONE_NEWUTS),
#endif
#ifdef CLONE_NEWIPC
    ALCOVE_DEFINE(CLONE_NEWIPC),
#endif
#ifdef CLONE_NEWUSER
    ALCOVE_DEFINE(CLONE_NEWUSER),
#endif
#ifdef CLONE_NEWPID
    ALCOVE_DEFINE(CLONE_NEWPID),
#endif
#ifdef CLONE_NEWNET
    ALCOVE_DEFINE(CLONE_NEWNET),
#endif
    {NULL, 0}
};
