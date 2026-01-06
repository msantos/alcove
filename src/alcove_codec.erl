%%% Copyright (c) 2014-2026, Michael Santos <michael.santos@gmail.com>
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
-export([decode/1]).
-export([stream/1]).

-type type() :: alcove_call | alcove_stdout | alcove_stderr | alcove_event | alcove_pipe.

-export_type([type/0]).

%%
%% Encode protocol terms to iodata
%%
-spec call(atom(), [alcove:pid_t()], [any()]) -> iodata().
call(Call, Pids, Arg) ->
    Bin =
        <<
            ?UINT16(?ALCOVE_MSG_CALL),
            ?UINT16(alcove_proto:call(Call)),
            (term_to_binary(list_to_tuple(Arg)))/binary
        >>,
    Size = byte_size(Bin),
    stdin(Pids, [<<?UINT16(Size)>>, Bin]).

-spec stdin([alcove:pid_t()], iodata()) -> iodata().
stdin(Pids, Data) ->
    lists:foldl(
        fun(Pid, Acc) ->
            Size = 2 + 4 + iolist_size(Acc),
            [<<?UINT16(Size), ?UINT16(?ALCOVE_MSG_STDIN), ?UINT32(Pid)>> | Acc]
        end,
        Data,
        lists:reverse(Pids)
    ).

%%
%% Decode protocol binary to term
%%
-spec stream(binary()) -> {[binary()], binary()}.
stream(Data) ->
    stream(Data, []).

stream(Data, Acc) ->
    case message(Data) of
        {<<>>, Rest} ->
            {lists:reverse(Acc), Rest};
        {Bin, Rest} ->
            stream(Rest, [Bin | Acc])
    end.

-spec message(binary()) -> {binary(), binary()}.
message(<<?UINT16(Len), Data/binary>> = Bin) when Len =< byte_size(Data) ->
    Size = Len + 2,
    <<Msg:Size/bytes, Rest/binary>> = Bin,
    {Msg, Rest};
message(Data) ->
    {<<>>, Data}.

-spec decode(binary()) -> {type(), [alcove:pid_t()], term()}.
decode(Msg) ->
    decode(Msg, []).

decode(<<?UINT16(Len), ?UINT16(?ALCOVE_MSG_PROXY), ?UINT32(Pid), Data/binary>>, Pids) when
    Len =:= 2 + 4 + byte_size(Data)
->
    decode(Data, [Pid | Pids]);
decode(<<?UINT16(Len), ?UINT16(?ALCOVE_MSG_STDOUT), ?UINT32(Pid), Data/binary>>, Pids) when
    Len =:= 2 + 4 + byte_size(Data)
->
    {alcove_stdout, lists:reverse([Pid | Pids]), Data};
decode(<<?UINT16(Len), ?UINT16(?ALCOVE_MSG_STDERR), ?UINT32(Pid), Data/binary>>, Pids) when
    Len =:= 2 + 4 + byte_size(Data)
->
    {alcove_stderr, lists:reverse([Pid | Pids]), Data};
decode(<<?UINT16(Len), ?UINT16(?ALCOVE_MSG_CALL), Data/binary>>, Pids) when
    Len =:= 2 + byte_size(Data)
->
    {alcove_call, lists:reverse(Pids), binary_to_term(Data)};
decode(<<?UINT16(Len), ?UINT16(?ALCOVE_MSG_EVENT), Data/binary>>, Pids) when
    Len =:= 2 + byte_size(Data)
->
    {alcove_event, lists:reverse(Pids), binary_to_term(Data)};
decode(<<?UINT16(Len), ?UINT16(?ALCOVE_MSG_CTL), Data/binary>>, Pids) when
    Len =:= 2 + byte_size(Data)
->
    {alcove_ctl, lists:reverse(Pids), binary_to_term(Data)};
decode(<<?UINT16(Len), ?UINT16(?ALCOVE_MSG_PIPE), Data/binary>>, Pids) when
    Len =:= 2 + byte_size(Data)
->
    {alcove_pipe, lists:reverse(Pids), binary_to_term(Data)}.
