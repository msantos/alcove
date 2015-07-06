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
typedef struct {
    int ctl[2];
    int in[2];
    int out[2];
    int err[2];
} alcove_stdio_t;

typedef struct {
    alcove_state_t *ap;
    alcove_stdio_t *fd;
    sigset_t *sigset;
} alcove_arg_t;

int alcove_stdio(alcove_stdio_t *fd);
int alcove_set_cloexec(int fd);
int alcove_close_pipe(int fd[2]);
int alcove_close_fd(int fd);
int alcove_child_fun(void *arg);
int alcove_parent_fd(alcove_state_t *ap, alcove_stdio_t *fd, pid_t pid);
int avail_pid(alcove_state_t *ap, alcove_child_t *c, void *arg1, void *arg2);
int stdio_pid(alcove_state_t *ap, alcove_child_t *c, void *arg1, void *arg2);
int close_parent_fd(alcove_state_t *ap, alcove_child_t *c,
                void *arg1, void *arg2);
