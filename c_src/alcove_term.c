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

    ETERM *
alcove_tuple2(ETERM *tag, ETERM *term)
{
    ETERM *t[2] = {tag, term};

    return erl_mk_tuple(t, 2);
}

    ETERM *
alcove_tuple3(ETERM *t1, ETERM *t2, ETERM *t3)
{
    ETERM *t[3] = {t1, t2, t3};

    return erl_mk_tuple(t, 3);
}

    ETERM *
alcove_tuple4(ETERM *t1, ETERM *t2, ETERM *t3, ETERM *t4)
{
    ETERM *t[4] = {t1, t2, t3, t4};

    return erl_mk_tuple(t, 4);
}

    ETERM *
alcove_tuple5(ETERM *t1, ETERM *t2, ETERM *t3, ETERM *t4, ETERM *t5)
{
    ETERM *t[5] = {t1, t2, t3, t4, t5};

    return erl_mk_tuple(t, 5);
}

    ETERM *
alcove_errno(int errnum)
{
    return alcove_error(erl_errno_id(errnum));
}

    ETERM *
alcove_error(const char *reason)
{
    return alcove_tuple2(erl_mk_atom("error"), erl_mk_atom(reason));
}

    ETERM *
alcove_ok(ETERM *term)
{
    return alcove_tuple2(erl_mk_atom("ok"), term);
}

    ETERM *
alcove_bool(bool ok)
{
    return (ok ? erl_mk_atom("true") : erl_mk_atom("false"));
}

    ETERM *
alcove_bin(const char *buf)
{
    return (buf ? erl_mk_binary(buf, strlen(buf)) : erl_mk_binary("",0));
}

    void *
alcove_malloc(ssize_t size)
{
    void *buf = NULL;

    if (size < 0 || size >= INT32_MAX)
        erl_err_quit("malloc:invalid size:%ld",
                (unsigned long)size);

    buf = erl_malloc(size);

    if (!buf)
        erl_err_sys("malloc");

    return buf;
}

    ETERM *
alcove_define(char *name, alcove_define_t *constants)
{
    alcove_define_t *dp = NULL;

    for (dp = constants; dp->name != NULL; dp++) {
        if (!strncmp(name, dp->name, strlen(name)))
            return erl_mk_ulonglong(dp->val);
    }

    return erl_mk_atom("false");
}

    ETERM *
alcove_constant(u_int64_t val, alcove_define_t *constants)
{
    alcove_define_t *dp = NULL;

    for (dp = constants; dp->name != NULL; dp++) {
        if (val == dp->val)
            return erl_mk_atom(dp->name);
    }

    return erl_mk_atom("false");
}

    ETERM *
alcove_list_head(ETERM **hd, ETERM *list)
{
    *hd = erl_hd(list);
    return erl_tl(list);
}

    char **
alcove_list_to_argv(ETERM *arg)
{
    ETERM *hd = NULL;
    ssize_t len = 0;
    int i = 0;
    char **argv = NULL;
    long maxarg = sysconf(_SC_ARG_MAX);

    len = erl_length(arg);

    if (len < 0 || len >= maxarg)
        return NULL;

    /* NULL terminate */
    argv = calloc(len + 1, sizeof(char **));

    if (!argv)
        erl_err_sys("calloc");

    for (i = 0; i < len; i++) {
        arg = alcove_list_head(&hd, arg);
        if (!hd)
            goto ERR;

        argv[i] = erl_iolist_to_string(hd);
        if (!argv[i])
            goto ERR;
    }

    return argv;

ERR:
    free(argv);
    return NULL;
}

    void
alcove_free_argv(char **argv)
{
    int i = 0;

    if (argv == NULL)
        return;

    for (i = 0; argv[i]; i++)
        free(argv[i]);

    free(argv);
}

    void *
alcove_list_to_buf(ETERM *arg, size_t *len, alcove_alloc_t **ptr, ssize_t *nptr)
{
    ETERM *hd = NULL;
    ETERM *parg = arg;
    ETERM *t = NULL;
    ssize_t nelem = 0;
    int i = 0;
    char *buf = NULL;
    char *pbuf = NULL;
    size_t n = 0;

    *len = 0;
    *nptr = 0;
    nelem = erl_length(arg);

    if (nelem < 0 || nelem >= 0xffff)
        return NULL;

    /* Calculate the size required */
    for (i = 0; i < nelem; i++) {
        parg = alcove_list_head(&hd, parg);
        if (!hd)
            return NULL;

        if (ERL_IS_BINARY(hd)) {
            n += erl_size(hd);
        }
        else if (ERL_IS_TUPLE(hd) && erl_size(hd) == 2) {
            t = erl_element(1, hd);
            if (!t || !ERL_IS_ATOM(t) || strncmp(ERL_ATOM_PTR(t), "ptr", 3))
                return NULL;

            t = erl_element(2, hd);

            if (ALCOVE_IS_UNSIGNED_LONG(t) || ERL_IS_BINARY(t)) {
                n += sizeof(void *);
            }
            else
                return NULL;
        }
        else
            return NULL;
    }

    buf = alcove_malloc(n);
    *len = n;

    *ptr = alcove_malloc(nelem * sizeof(alcove_alloc_t));
    *nptr = nelem;

    pbuf = buf;

    /* Copy the list contents */
    for (i = 0; i < nelem; i++) {
        arg = alcove_list_head(&hd, arg);

        if (ERL_IS_BINARY(hd)) {
            (void)memcpy(buf, ERL_BIN_PTR(hd), ERL_BIN_SIZE(hd));
            buf += ERL_BIN_SIZE(hd);
            (*ptr)[i].p = NULL;
            (*ptr)[i].len = ERL_BIN_SIZE(hd);
        }
        else if (ERL_IS_TUPLE(hd)) {
            t = erl_element(2, hd);

            if (ALCOVE_IS_UNSIGNED_LONG(t)) {
                char *p = calloc(ALCOVE_LL_UVALUE(t), 1);
                if (!p)
                    erl_err_sys("calloc");

                (void)memcpy(buf, &p, sizeof(void *));
                buf += sizeof(void *);

                (*ptr)[i].p = p;
                (*ptr)[i].len = ALCOVE_LL_UVALUE(t);
            }
            else if (ERL_IS_BINARY(t)) {
                char *p = alcove_malloc(ERL_BIN_SIZE(t));
                (void)memcpy(p, ERL_BIN_PTR(t), ERL_BIN_SIZE(t));
                (void)memcpy(buf, &p, sizeof(void *));
                buf += sizeof(void *);
                (*ptr)[i].p = p;
                (*ptr)[i].len = ERL_BIN_SIZE(t);
            }
        }
    }

    return pbuf;
}

    ETERM *
alcove_buf_to_list(char *buf, size_t len, alcove_alloc_t *ptr, ssize_t nptr)
{
    ETERM *t = NULL;

    t = erl_mk_empty_list();
    for (nptr-- ; nptr >= 0; nptr--) {
        if (ptr[nptr].p) {
            /* Allocated buffer */
            len -= sizeof(void *);
            t = erl_cons(alcove_tuple2(erl_mk_atom("ptr"),
                        erl_mk_binary(ptr[nptr].p, ptr[nptr].len)), t);
            free(ptr[nptr].p);
        }
        else {
            /* Static binary */
            len -= ptr[nptr].len;
            t = erl_cons(erl_mk_binary(buf+len, ptr[nptr].len), t);
        }
    }

    return t;
}
