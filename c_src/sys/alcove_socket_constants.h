/* Copyright (c) 2016, Michael Santos <michael.santos@gmail.com>
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
static const alcove_constant_t alcove_socket_constants[] = {
#ifdef AF_APPLETALK
    ALCOVE_CONSTANT(AF_APPLETALK),
#endif
#ifdef AF_ATMPVC
    ALCOVE_CONSTANT(AF_ATMPVC),
#endif
#ifdef AF_AX25
    ALCOVE_CONSTANT(AF_AX25),
#endif
#ifdef AF_INET
    ALCOVE_CONSTANT(AF_INET),
#endif
#ifdef AF_INET6
    ALCOVE_CONSTANT(AF_INET6),
#endif
#ifdef AF_IPX
    ALCOVE_CONSTANT(AF_IPX),
#endif
#ifdef AF_LOCAL
    ALCOVE_CONSTANT(AF_LOCAL),
#endif
#ifdef AF_NETLINK
    ALCOVE_CONSTANT(AF_NETLINK),
#endif
#ifdef AF_PACKET
    ALCOVE_CONSTANT(AF_PACKET),
#endif
#ifdef AF_UNIX
    ALCOVE_CONSTANT(AF_UNIX),
#endif
#ifdef AF_X25
    ALCOVE_CONSTANT(AF_X25),
#endif
#ifdef PF_APPLETALK
    ALCOVE_CONSTANT(PF_APPLETALK),
#endif
#ifdef PF_ATMPVC
    ALCOVE_CONSTANT(PF_ATMPVC),
#endif
#ifdef PF_AX25
    ALCOVE_CONSTANT(PF_AX25),
#endif
#ifdef PF_INET
    ALCOVE_CONSTANT(PF_INET),
#endif
#ifdef PF_INET6
    ALCOVE_CONSTANT(PF_INET6),
#endif
#ifdef PF_IPX
    ALCOVE_CONSTANT(PF_IPX),
#endif
#ifdef PF_LOCAL
    ALCOVE_CONSTANT(PF_LOCAL),
#endif
#ifdef PF_NETLINK
    ALCOVE_CONSTANT(PF_NETLINK),
#endif
#ifdef PF_PACKET
    ALCOVE_CONSTANT(PF_PACKET),
#endif
#ifdef PF_UNIX
    ALCOVE_CONSTANT(PF_UNIX),
#endif
#ifdef PF_X25
    ALCOVE_CONSTANT(PF_X25),
#endif
#ifdef SOCK_CLOEXEC
    ALCOVE_CONSTANT(SOCK_CLOEXEC),
#endif
#ifdef SOCK_DGRAM
    ALCOVE_CONSTANT(SOCK_DGRAM),
#endif
#ifdef SOCK_NONBLOCK
    ALCOVE_CONSTANT(SOCK_NONBLOCK),
#endif
#ifdef SOCK_PACKET
    ALCOVE_CONSTANT(SOCK_PACKET),
#endif
#ifdef SOCK_RAW
    ALCOVE_CONSTANT(SOCK_RAW),
#endif
#ifdef SOCK_RDM
    ALCOVE_CONSTANT(SOCK_RDM),
#endif
#ifdef SOCK_SEQPACKET
    ALCOVE_CONSTANT(SOCK_SEQPACKET),
#endif
#ifdef SOCK_STREAM
    ALCOVE_CONSTANT(SOCK_STREAM),
#endif

    {NULL, 0}
};