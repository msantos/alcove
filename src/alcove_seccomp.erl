%%% Copyright (c) 2014, Michael Santos <michael.santos@gmail.com>
%%%
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
%-module(alcove_seccomp).
-module(alcove_seccomp).
-include_lib("alcove/include/alcove_seccomp.hrl").

-export([
        insn/1,
        stmt/2,
        jump/4,
        offset/1
    ]).

%%-------------------------------------------------------------------------
%%% BPF filtering
%%-------------------------------------------------------------------------
insn(#alcove_insn{
        code = Code,
        jt = JT,
        jf = JF,
        k = K
    }) ->
    <<Code:2/native-unsigned-integer-unit:8,
    JT:8, JF:8,
    K:4/native-unsigned-integer-unit:8>>;
insn(<<Code:2/native-unsigned-integer-unit:8,
    JT:8, JF:8,
    K:4/native-unsigned-integer-unit:8>>) ->
    #alcove_insn{
        code = Code,
        jt = JT,
        jf = JF,
        k = K
    }.

stmt(Code, K) when is_integer(Code), is_integer(K) ->
    insn(#alcove_insn{
        code = Code,
        k = K
    }).

jump(Code, K, JT, JF) when is_integer(Code), is_integer(K),
    is_integer(JT), is_integer(JF) ->
    insn(#alcove_insn{
        code = Code,
        jt = JT,
        jf = JF,
        k = K
    }).

offset(word) -> ?BPF_W;
offset(halfword) -> ?BPF_H;
offset(byte) -> ?BPF_B;

offset(?BPF_W) -> word;
offset(?BPF_H) -> halfword;
offset(?BPF_B) -> byte.
