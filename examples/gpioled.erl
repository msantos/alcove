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
-record(state, {
        drv,
        pid,
        gpio,
        direction,
        unexport
    }).

start(GPIO) ->
    start(GPIO, 1000).
start(GPIO, N) ->
    {ok, Drv} = alcove_drv:start_link([{exec, "sudo"}]),

    export(Drv, GPIO),

    % Child
    Flags = [
        clone_newipc,
        clone_newns,
        clone_newnet,
        clone_newpid,
        clone_newuts
    ],
    {ok, Child} = alcove:clone(Drv, [], Flags),

    ok = alcove:chroot(Drv, [Child],
        ["/sys/class/gpio/gpio", integer_to_list(GPIO)]),
    ok = alcove:chdir(Drv, [Child], "/"),
    {ok, FD} = alcove:open(Drv, [Child], "/direction", [o_rdwr,o_cloexec], 0),

    Id = id(),
    ok = alcove:setgid(Drv, [Child], Id),
    ok = alcove:setuid(Drv, [Child], Id),

    % Drop privs in the port
    {ok, UFD} = alcove:open(Drv, [], "/sys/class/gpio/unexport",
        [o_wronly,o_cloexec], 0),

    ok = alcove:unshare(Drv, [], Flags),
    ok = alcove:chroot(Drv, [], "priv"),
    ok = alcove:chdir(Drv, [], "/"),

    Id1 = id(),
    ok = alcove:setgid(Drv, [], Id1),
    ok = alcove:setuid(Drv, [], Id1),

    strobe(#state{
            drv = Drv,
            pid = Child,
            gpio = GPIO,
            direction = FD,
            unexport = UFD
        }, N).


strobe(#state{drv = Drv, pid = Child, direction = FD,
        unexport = UFD, gpio = GPIO}, 0) ->
    alcove:write(Drv, [Child], FD, <<"low">>),
    alcove:close(Drv, [Child], FD),
    {ok, _} = alcove:write(Drv, [], UFD, integer_to_list(GPIO)),
    alcove:close(Drv, [], UFD),
    alcove_drv:stop(Drv);
strobe(#state{drv = Drv, pid = Child, direction = FD} = State, N) ->
    timer:sleep(100),
    alcove:write(Drv, [Child], FD, <<"low">>),
    timer:sleep(100),
    alcove:write(Drv, [Child], FD, <<"high">>),
    strobe(State, N-1).

id() ->
    crypto:rand_uniform(16#f0000000, 16#f000ffff).

export(Drv, Pin) ->
    {ok, FD} = alcove:open(Drv, [], "/sys/class/gpio/export",
        [o_wronly,o_cloexec], 0),
    case alcove:write(Drv, [], FD, integer_to_list(Pin)) of
        {ok, _} -> ok;
        {error,ebusy} -> ok
    end,
    alcove:close(Drv, [], FD).
