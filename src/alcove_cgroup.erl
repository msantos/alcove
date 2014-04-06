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
-module(alcove_cgroup).
-include_lib("alcove/include/alcove.hrl").

-export([supported/1, supported/2]).
-export([create/1, create/2, create/3, destroy/1, destroy/2, destroy/3]).
-export([cgroup/1, cgroup/2, fold/5, fold/6, fold_files/6, fold_files/7]).
-export([get/5, set/6]).
-export([mounts/1, mounts/2]).

supported(Port) ->
    supported(Port, []).

supported(Port, Pids) ->
    foreach([
        fun() -> {unix,linux} =:= os:type() end,
        fun() -> [] =/= cgroup(Port, Pids) end,
        fun() -> ok =:= element(1, get(Port, Pids, <<"cpuset">>, <<>>,
                        <<"notify_on_release">>)) end
    ]).

%% These functions perform file operations in the port rather than using
%% the stdlib because:
%%
%%  * the port may be running with different privileges than beam (as
%%    a different user, ...)
%%
%%  * the process may be running in a chroot or a different mount namespace
%%

create(Port) ->
    create(Port, []).
create(Port, Pids) ->
    create(Port, Pids, <<"alcove">>).
create(Port, Pids, Namespace) ->
    Fun = fun(Cgroup, _Acc) ->
            Path = join(Cgroup, Namespace),
            case alcove:mkdir(Port, Pids, Path, 8#755) of
                ok -> ok;
                {error,eexist} -> ok;
                Error -> Error
            end
    end,
    fold(Port, <<>>, <<>>, Fun, []).

destroy(Port) ->
    destroy(Port, []).
destroy(Port, Pids) ->
    destroy(Port, Pids, <<"alcove">>).
destroy(Port, Pids, Namespace) ->
    Fun = fun(Cgroup, _Acc) ->
            Path = join(Cgroup, Namespace),
            case alcove:rmdir(Port, Pids, Path) of
                ok -> ok;
                {error,enoent} -> ok;
                Error -> Error
            end
    end,
    fold(Port, <<>>, <<>>, Fun, []).

set(Port, Pids, Type, Namespace, Key, Value) ->
    Fun = fun(Cgroup, _Acc) ->
            File = join(Cgroup, Key),
            write(Port, Pids, File, Value)
    end,
    fold(Port, Type, Namespace, Fun, []).

get(Port, Pids, Type, Namespace, Key) ->
    Fun = fun(Cgroup, _Acc) ->
            File = join(Cgroup, Key),
            read(Port, Pids, File)
    end,
    fold(Port, Type, Namespace, Fun, []).

write(Port, Pids, File, Bytes) ->
    Flags = alcove:define(Port, 'O_WRONLY'),
    Reply = case alcove:open(Port, Pids, File, Flags, 0) of
        {ok, FH} ->
            N = alcove:write(Port, Pids, FH, Bytes),
            alcove:close(Port, Pids, FH),
            N;
        Error ->
            Error
    end,
    Reply.

read(Port, Pids, File) ->
    Flags = alcove:define(Port, 'O_RDONLY'),
    Reply = case alcove:open(Port, Pids, File, Flags, 0) of
        {ok, FH} ->
            N = readbuf(Port, Pids, FH),
            alcove:close(Port, Pids, FH),
            N;
        Error ->
            Error
    end,
    Reply.

fold(Port, Type, Namespace, Fun, AccIn) ->
    fold(Port, [], Type, Namespace, Fun, AccIn).

fold(Port, Pids, Type, Namespace, Fun, AccIn) ->
    Cgroups = lists:foldl(fun({Cgroup, Opt}, Acc) ->
                Path = join(Cgroup, Namespace),
                case is_dir(Port, Pids, Path) of
                    true ->
                        case {Type, lists:member(Type,Opt)} of
                            {_, true} -> [Path|Acc];
                            {<<>>, _} -> [Path|Acc];
                            {_, false} -> Acc
                        end;
                    false ->
                        Acc
                end
        end, [], cgroup(Port, Pids)),

    lists:foldl(Fun, AccIn, Cgroups).

fold_files(Port, Type, Namespace, RegExp, Fun, AccIn) ->
    fold_files(Port, [], Type, Namespace, RegExp, Fun, AccIn).

fold_files(Port, Pids, Type, Namespace, RegExp, CallerFun, AccIn) ->
    {ok, MP} = re:compile(RegExp),

    Fun = fun(Dir, Acc) ->
            {ok, Fs} = alcove:readdir(Port, Dir),
            Filtered = lists:filter(fun(File) ->
                        case re:run(File, MP) of
                            nomatch ->
                                false;
                            {match, _} ->
                                true
                        end
                end, Fs),
            lists:foldl(CallerFun, Acc, Filtered)
        end,

    fold(Port, Pids, Type, Namespace, Fun, AccIn).

cgroup(Port) ->
    cgroup(Port, []).
cgroup(Port, Pids) ->
    {ok, Entries} = mounts(Port, Pids),
    [ {Dir, binary:split(MntOpts, [<<",">>], [global])} ||
        {<<"cgroup">>, Dir, <<"cgroup">>, MntOpts, _Freq, _Passno} <- Entries ].

mounts(Port) ->
    mounts(Port, []).
mounts(Port, Pids) ->
    Flags = alcove:define(Port, 'O_RDONLY'),
    {ok, Fd} = alcove:open(Port, Pids, "/proc/mounts", Flags, 0),
    Reply = case readbuf(Port, Pids, Fd) of
        {ok, Buf} ->
            {ok, fsentry(Buf)};
        Error ->
            Error
    end,
    alcove:close(Port, Pids, Fd),
    Reply.

readbuf(Port, Pids, Fd) ->
    readbuf(Port, Pids, Fd, []).
readbuf(Port, Pids, Fd, Acc) ->
    case alcove:read(Port, Pids, Fd, 1024) of
        {ok, <<>>} ->
            {ok, list_to_binary(lists:reverse(Acc))};
        {ok, Buf} ->
            readbuf(Port, Pids, Fd, [Buf|Acc]);
        Error ->
            Error
    end.

fsentry(Buf) ->
    Entries = binary:split(Buf, [<<"\n">>], [global,trim]),
    [ list_to_tuple(binary:split(Entry, [<<"\s">>], [global])) ||
        Entry <- Entries ].

is_dir(Port, Pids, Path) ->
    Flags = alcove:define(Port, 'O_WRONLY'),
    case alcove:open(Port, Pids, Path, Flags, 0) of
        {error,eisdir} -> true;
        {error,_} -> false;
        {ok, FH} ->
            alcove:close(Port, Pids, FH),
            false
    end.

is_file(Port, Pids, File) ->
    Flags = alcove:define(Port, file, ['O_RDONLY']),
    case alcove:open(Port, Pids, File, Flags, 0) of
        {ok, FH} ->
            alcove:close(Port, FH),
            true;
        _ ->
            false
    end.

join(Cgroup, Path) ->
    filename:join([Cgroup|relpath(Path)]).

relpath(Path) ->
    case filename:split(Path) of
        [Root|Rest] when Root =:= "/"; Root =:= <<"/">> ->
            Rest;
        Rest ->
            Rest
    end.

foreach([]) ->
    true;
foreach([Fun|Funs]) ->
    case Fun() of
        true -> foreach(Funs);
        false -> false
    end.
