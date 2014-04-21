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
-module(gpioled).
-export([start/1, start/2]).

%% Beaglebone black:
%%
%% You'll need a kernel that supports namespaces.
%%
%% The LED should be plugged into GPIO 7 (labelled P9_42A) and the GND (P9_44)
%% beside it. See:
%%
%%  http://eskimon.fr/beaglebone-black-gpio-interactive-map
%%
%% Also see this tutorial on which this example is based:
%%
%%  http://www.linux.com/learn/tutorials/725368-getting-started-with-the-beaglebone-black-a-1ghz-arm-linux-machine-for-45
%%
%%  gpioled:start(7).
%%
%% Raspberry Pi:
%%
%% The stock raspbian supports namespaces.
%%
%% Use GPIO  11.
%%
%%  gpioled:start(11).
%%
-define(O_WRONLY, alcove:define(Port, 'O_WRONLY')).
-define(O_RDWR, alcove:define(Port, 'O_RDWR')).

start(GPIO) ->
    start(GPIO, 1000).
start(GPIO, N) ->
    Port = alcove_drv:start([{exec, "sudo"}]),

    export(Port, GPIO),

    Flags = alcove:define(Port, [
            'CLONE_NEWIPC',
            'CLONE_NEWNS',
            'CLONE_NEWNET',
            'CLONE_NEWPID',
            'CLONE_NEWUTS'
        ]),

    % Child
    {ok, Child} = alcove:clone(Port, Flags),
    ok = alcove:chroot(Port, [Child],
        ["/sys/class/gpio/gpio", integer_to_list(GPIO)]),
    ok = alcove:chdir(Port, [Child], "/"),
    {ok, FD} = alcove:open(Port, [Child], "/direction", ?O_RDWR, 0),

    Id = id(),
    ok = alcove:setgid(Port, [Child], Id),
    ok = alcove:setuid(Port, [Child], Id),

    % Drop privs in the port
    {ok, UFD} = alcove:open(Port, "/sys/class/gpio/unexport", ?O_WRONLY, 0),

    ok = alcove:unshare(Port, Flags),
    ok = alcove:chroot(Port, "priv"),
    ok = alcove:chdir(Port, "/"),

    Id1 = id(),
    ok = alcove:setgid(Port, Id1),
    ok = alcove:setuid(Port, Id1),

    strobe(Port, Child, FD, N),

    {ok, _} = alcove:write(Port, UFD, integer_to_list(GPIO)),
    alcove:close(Port, UFD),

    alcove_drv:stop(Port).

strobe(Port, Child, FD, 0) ->
    alcove:write(Port, [Child], FD, <<"low">>),
    alcove:close(Port, [Child], FD);
strobe(Port, Child, FD, N) ->
    timer:sleep(100),
    alcove:write(Port, [Child], FD, <<"low">>),
    timer:sleep(100),
    alcove:write(Port, [Child], FD, <<"high">>),
    strobe(Port, Child, FD, N-1).

id() ->
    16#f0000000 + crypto:rand_uniform(0, 16#ffff).

export(Port, Pin) ->
    {ok, FD} = alcove:open(Port, "/sys/class/gpio/export", ?O_WRONLY, 0),
    case alcove:write(Port, FD, integer_to_list(Pin)) of
        {ok, _} -> ok;
        {error,ebusy} -> ok
    end,
    alcove:close(Port, FD).
