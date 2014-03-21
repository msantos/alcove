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
-record(alcove, {
        port :: port()
    }).

-record(rlimit, {
        cur = 0 :: non_neg_integer(),
        max = 0 :: non_neg_integer()
    }).

-define(UINT16(N), N:2/unsigned-integer-unit:8).
-define(UINT32(N), N:4/unsigned-integer-unit:8).

-define(ALCOVE_MSG_CALL, 0).
-define(ALCOVE_MSG_EVENT, 1).
-define(ALCOVE_MSG_STDIN, 2).
-define(ALCOVE_MSG_STDOUT, 3).
-define(ALCOVE_MSG_STDERR, 4).

-define(ALCOVE_HDR(Type, Pid0),
    ?UINT16(Type), ?UINT32(Pid0)).

-define(ALCOVE_HDR(Type, Pid0, Pid1),
    ?UINT16(?ALCOVE_MSG_STDOUT), ?UINT32(Pid0),
    ?UINT16(_), ?UINT16(Type), ?UINT32(Pid1)).

-define(ALCOVE_HDR(Type, Pid0, Pid1, Pid2),
    ?UINT16(?ALCOVE_MSG_STDOUT), ?UINT32(Pid0),
    ?UINT16(_), ?UINT16(?ALCOVE_MSG_STDOUT), ?UINT32(Pid1),
    ?UINT16(_), ?UINT16(Type), ?UINT32(Pid2)).

-define(ALCOVE_HDR(Type, Pid0, Pid1, Pid2, Pid3),
    ?UINT16(?ALCOVE_MSG_STDOUT), ?UINT32(Pid0),
    ?UINT16(_), ?UINT16(?ALCOVE_MSG_STDOUT), ?UINT32(Pid1),
    ?UINT16(_), ?UINT16(?ALCOVE_MSG_STDOUT), ?UINT32(Pid2),
    ?UINT16(_), ?UINT16(Type), ?UINT32(Pid3)).

-define(ALCOVE_HDR(Type, Pid0, Pid1, Pid2, Pid3, Pid4),
    ?UINT16(?ALCOVE_MSG_STDOUT), ?UINT32(Pid0),
    ?UINT16(_), ?UINT16(?ALCOVE_MSG_STDOUT), ?UINT32(Pid1),
    ?UINT16(_), ?UINT16(?ALCOVE_MSG_STDOUT), ?UINT32(Pid2),
    ?UINT16(_), ?UINT16(?ALCOVE_MSG_STDOUT), ?UINT32(Pid3),
    ?UINT16(_), ?UINT16(Type), ?UINT32(Pid4)).
