%%% Copyright (c) 2015, Michael Santos <michael.santos@gmail.com>
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
-module(alcove_codec_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("alcove/include/alcove.hrl").

-export([
    all/0
]).

-export([
    decode/1
]).

all() ->
    [decode].

%%
%% Tests
%%

decode(_Config) ->
    % Length, Message type, Term
    Msg = <<
        0,
        37,
        0,
        3,
        0,
        0,
        1,
        39,
        0,
        29,
        0,
        3,
        0,
        0,
        2,
        39,
        0,
        21,
        0,
        3,
        0,
        0,
        3,
        39,
        0,
        13,
        0,
        4,
        131,
        109,
        0,
        0,
        0,
        5,
        48,
        46,
        50,
        46,
        48
    >>,

    {alcove_call, [295, 551, 807], <<"0.2.0">>} = alcove_codec:decode(Msg).
