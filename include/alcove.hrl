% Copyright (c) 2014, Michael Santos <michael.santos@gmail.com>
% Permission to use, copy, modify, and/or distribute this software for any
% purpose with or without fee is hereby granted, provided that the above
% copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
-record(alcove_rlimit, {
        cur = 0 :: alcove:uint64_t(),
        max = 0 :: alcove:uint64_t()
    }).

-record(alcove_pid, {
        pid = 0 :: non_neg_integer(),
        fdctl = -1 :: integer(),
        stdin = -1 :: integer(),
        stdout = -1 :: integer(),
        stderr = -1 :: integer()
    }).

-record(alcove_timeval, {
        sec = 0 :: non_neg_integer(),
        usec = 0 :: non_neg_integer()
    }).

-record(alcove_jail, {
        version = 2 :: alcove:uint32_t(),
        path = <<>> :: iodata(),
        hostname = <<>> :: iodata(),
        jailname = <<>> :: iodata(),
        ip4 = [] :: [inet:ip4_address()],
        ip6 = [] :: [inet:ip6_address()]
    }).

-define(UINT16(N), (N):2/unsigned-integer-unit:8).
-define(UINT32(N), (N):4/unsigned-integer-unit:8).

-define(ALCOVE_MSG_STDIN, 0).
-define(ALCOVE_MSG_STDOUT, 1).
-define(ALCOVE_MSG_STDERR, 2).
-define(ALCOVE_MSG_PROXY, 3).
-define(ALCOVE_MSG_CALL, 4).
-define(ALCOVE_MSG_EVENT, 5).
-define(ALCOVE_MSG_CTL, 6).
