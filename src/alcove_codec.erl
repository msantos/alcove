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
-module(alcove_codec).
-include_lib("alcove/include/alcove.hrl").

-export([call/3, stdin/2]).
-export([encode/2, encode/3, decode/1]).

call(Call, Pids, Arg) ->
    Bin = encode(alcove_proto:call(Call), Arg),
    Payload = case Pids of
        [] ->
            Bin;
        _ ->
            Size = iolist_size(Bin),
            [<<?UINT16(Size)>>, Bin]
    end,
    stdin(Pids, Payload).

stdin([], Data) ->
    Data;
stdin(Pids, Data) ->
    hdr(Pids, Data).

hdr(Pids, Data) ->
    hdr_1(lists:reverse(Pids), Data).

hdr_1([], [_Length|Acc]) ->
    Acc;
hdr_1([Pid|Pids], Acc) ->
    Size = iolist_size(Acc) + 2 + 4,
    hdr_1(Pids, [<<?UINT16(Size)>>, <<?UINT16(?ALCOVE_MSG_STDIN)>>, <<?UINT32(Pid)>>|Acc]).

encode(Command, Arg) when is_integer(Command), is_list(Arg) ->
    encode(?ALCOVE_MSG_CALL, Command, Arg).
encode(Type, Command, Arg) when is_integer(Type), is_integer(Command), is_list(Arg) ->
    <<
    ?UINT16(Type),
    ?UINT16(Command),
    (term_to_binary(list_to_tuple(Arg)))/binary
    >>.

decode(Msg) ->
    % Re-add the message length stripped off by open_port
    Len = byte_size(Msg),
    decode(<<?UINT16(Len), Msg/binary>>, [], [], []).

decode(<<>>, _Pids, Msg, Acc) ->
    lists:flatten(lists:reverse([Msg|Acc]));

decode(<<?UINT16(Len), ?UINT16(?ALCOVE_MSG_PROXY), ?UINT32(Pid),
    Rest/binary>>, Pids, [], Acc) when Len =:= 2 + 4 + byte_size(Rest) ->
    decode(Rest, [Pid|Pids], [], Acc);
decode(<<?UINT16(Len), ?UINT16(?ALCOVE_MSG_PROXY), ?UINT32(Pid),
    Rest/binary>>, _Pids, Msg, Acc) when Len =:= 2 + 4 + byte_size(Rest) ->
    decode(Rest, [Pid], [], [lists:reverse(Msg)|Acc]);

decode(<<?UINT16(Len), ?UINT16(?ALCOVE_MSG_STDOUT), ?UINT32(Pid),
    Bin/binary>>, Pids, Msg, Acc) ->
    Bytes = Len - (2 + 4),
    <<Reply:Bytes/bytes, Rest/binary>> = Bin,
    decode(Rest, Pids,
        [{alcove_stdout, lists:reverse([Pid|Pids]), Reply}|Msg], Acc);

decode(<<?UINT16(Len), ?UINT16(?ALCOVE_MSG_STDERR), ?UINT32(Pid),
    Bin/binary>>, Pids, Msg, Acc) ->
    Bytes = Len - (2 + 4),
    <<Reply:Bytes/bytes, Rest/binary>> = Bin,
    decode(Rest, Pids,
        [{alcove_stderr, lists:reverse([Pid|Pids]), Reply}|Msg], Acc);

decode(<<?UINT16(Len), ?UINT16(?ALCOVE_MSG_CALL),
    Bin/binary>>, Pids, Msg, Acc) ->
    Bytes = Len - 2,
    <<Reply:Bytes/bytes, Rest/binary>> = Bin,
    decode(Rest, Pids,
        [{alcove_call, lists:reverse(Pids), binary_to_term(Reply)}|Msg],
        Acc);

decode(<<?UINT16(Len), ?UINT16(?ALCOVE_MSG_EVENT),
    Bin/binary>>, Pids, Msg, Acc) ->
    Bytes = Len - 2,
    <<Reply:Bytes/bytes, Rest/binary>> = Bin,
    decode(Rest, Pids,
        [{alcove_event, lists:reverse(Pids), binary_to_term(Reply)}|Msg],
        Acc).
