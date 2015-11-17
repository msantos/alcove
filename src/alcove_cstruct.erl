%%% Copyright (c) 2015, Michael Santos <michael.santos@gmail.com>
%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
-module(alcove_cstruct).

-export([
        jail/1
    ]).

-spec jail({non_neg_integer(), iodata(), iodata(), iodata(),
            [inet:ip4_address()], [inet:ip6_address()]})
    -> [binary()|{ptr, binary()}].
jail({2, Path, Hostname, Jailname, IPv4, IPv6}) ->
    [<<2:4/native-unsigned-integer-unit:8>>,
     <<0:(alcove:wordalign(4) * 8)>>,
     {ptr, <<Path/binary, 0>>},
     {ptr, <<Hostname/binary, 0>>},
     {ptr, <<Jailname/binary, 0>>},
     <<(length(IPv4)):4/native-unsigned-integer-unit:8,
       (length(IPv6)):4/native-unsigned-integer-unit:8>>,
     {ptr, << <<IP1,IP2,IP3,IP4>> || {IP1,IP2,IP3,IP4} <- IPv4 >>},
     {ptr, << <<IP1:16,IP2:16,IP3:16,IP4:16,IP5:16,IP6:16,IP7:16,IP8:16>>
              || {IP1,IP2,IP3,IP4,IP5,IP6,IP7,IP8} <- IPv6 >>}].
