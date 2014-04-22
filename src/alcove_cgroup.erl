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
-export([is_file/3, is_dir/3]).
-export([mounts/1, mounts/2]).

supported(Drv) ->
    supported(Drv, []).

supported(Drv, Pids) ->
    foreach([
        fun() -> {unix,linux} =:= os:type() end,
        fun() -> [] =/= cgroup(Drv, Pids) end,
        fun() -> Val = get(Drv, Pids, <<"cpuset">>, <<>>,
                    <<"notify_on_release">>),
                is_tuple(Val) andalso ok =:= element(1, Val)
        end
    ]).

%% These functions perform file operations in the port rather than using
%% the stdlib because:
%%
%%  * the port may be running with different privileges than beam (as
%%    a different user, ...)
%%
%%  * the process may be running in a chroot or a different mount namespace
%%

create(Drv) ->
    create(Drv, []).
create(Drv, Pids) ->
    create(Drv, Pids, <<"alcove">>).
create(Drv, Pids, Namespace) ->
    Fun = fun(Cgroup, _Acc) ->
            Path = join(Cgroup, Namespace),
            case alcove:mkdir(Drv, Pids, Path, 8#755) of
                ok -> ok;
                {error,eexist} -> ok;
                Error -> Error
            end
    end,
    fold(Drv, <<>>, <<>>, Fun, []).

destroy(Drv) ->
    destroy(Drv, []).
destroy(Drv, Pids) ->
    destroy(Drv, Pids, <<"alcove">>).
destroy(Drv, Pids, Namespace) ->
    Fun = fun(Cgroup, _Acc) ->
            Path = join(Cgroup, Namespace),
            case alcove:rmdir(Drv, Pids, Path) of
                ok -> ok;
                {error,enoent} -> ok;
                Error -> Error
            end
    end,
    fold(Drv, <<>>, <<>>, Fun, []).

set(Drv, Pids, MntOpt, Namespace, Key, Value) ->
    Fun = fun(Cgroup, _Acc) ->
            File = join(Cgroup, Key),
            write(Drv, Pids, File, Value)
    end,
    fold(Drv, MntOpt, Namespace, Fun, []).

get(Drv, Pids, MntOpt, Namespace, Key) ->
    Fun = fun(Cgroup, _Acc) ->
            File = join(Cgroup, Key),
            read(Drv, Pids, File)
    end,
    fold(Drv, MntOpt, Namespace, Fun, []).

write(Drv, Pids, File, Bytes) ->
    Flags = alcove:define(Drv, 'O_WRONLY'),
    Reply = case alcove:open(Drv, Pids, File, Flags, 0) of
        {ok, FH} ->
            N = alcove:write(Drv, Pids, FH, Bytes),
            alcove:close(Drv, Pids, FH),
            N;
        Error ->
            Error
    end,
    Reply.

read(Drv, Pids, File) ->
    Flags = alcove:define(Drv, 'O_RDONLY'),
    Reply = case alcove:open(Drv, Pids, File, Flags, 0) of
        {ok, FH} ->
            N = readbuf(Drv, Pids, FH),
            alcove:close(Drv, Pids, FH),
            N;
        Error ->
            Error
    end,
    Reply.

fold(Drv, MntOpt, Namespace, Fun, AccIn) ->
    fold(Drv, [], MntOpt, Namespace, Fun, AccIn).

fold(Drv, Pids, MntOpt, Namespace, Fun, AccIn) ->
    Cgroups = lists:foldl(fun({Cgroup, Opt}, Acc) ->
                Path = join(Cgroup, Namespace),
                IsCgroup = (MntOpt =:= <<>> orelse lists:member(MntOpt,Opt))
                    andalso is_dir(Drv, Pids, Path),
                case IsCgroup of
                    true -> [Path|Acc];
                    false -> Acc
                end
        end, [], cgroup(Drv, Pids)),

    lists:foldl(Fun, AccIn, Cgroups).

fold_files(Drv, MntOpt, Namespace, RegExp, Fun, AccIn) ->
    fold_files(Drv, [], MntOpt, Namespace, RegExp, Fun, AccIn).

fold_files(Drv, Pids, MntOpt, Namespace, RegExp, CallerFun, AccIn) ->
    {ok, MP} = re:compile(RegExp),

    Fun = fun(Dir, Acc) ->
            {ok, Fs} = alcove:readdir(Drv, Pids, Dir),
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

    fold(Drv, Pids, MntOpt, Namespace, Fun, AccIn).

cgroup(Drv) ->
    cgroup(Drv, []).
cgroup(Drv, Pids) ->
    {ok, Entries} = mounts(Drv, Pids),
    [ {Dir, binary:split(MntOpts, [<<",">>], [global])} ||
        {<<"cgroup">>, Dir, <<"cgroup">>, MntOpts, _Freq, _Passno} <- Entries ].

mounts(Drv) ->
    mounts(Drv, []).
mounts(Drv, Pids) ->
    Flags = alcove:define(Drv, 'O_RDONLY'),
    case alcove:open(Drv, Pids, "/proc/mounts", Flags, 0) of
        {ok, FD} ->
            Reply = case readbuf(Drv, Pids, FD) of
                {ok, Buf} ->
                    {ok, fsentry(Buf)};
                Error ->
                    Error
            end,
            alcove:close(Drv, Pids, FD),
            Reply;
        _ ->
            {ok, []}
    end.

readbuf(Drv, Pids, FD) ->
    readbuf(Drv, Pids, FD, []).
readbuf(Drv, Pids, FD, Acc) ->
    case alcove:read(Drv, Pids, FD, 1024) of
        {ok, <<>>} ->
            {ok, list_to_binary(lists:reverse(Acc))};
        {ok, Buf} ->
            readbuf(Drv, Pids, FD, [Buf|Acc]);
        Error ->
            Error
    end.

fsentry(Buf) ->
    Entries = binary:split(Buf, [<<"\n">>], [global,trim]),
    [ list_to_tuple(binary:split(Entry, [<<"\s">>], [global])) ||
        Entry <- Entries ].

is_dir(Drv, Pids, Path) ->
    Flags = alcove:define(Drv, 'O_WRONLY'),
    case alcove:open(Drv, Pids, Path, Flags, 0) of
        {error,eisdir} -> true;
        {error,_} -> false;
        {ok, FH} ->
            alcove:close(Drv, Pids, FH),
            false
    end.

is_file(Drv, Pids, File) ->
    Flags = alcove:define(Drv, ['O_RDONLY']),
    case alcove:open(Drv, Pids, File, Flags, 0) of
        {ok, FH} ->
            alcove:close(Drv, FH),
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
