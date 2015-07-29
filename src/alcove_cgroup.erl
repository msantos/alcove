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

-export([supported/2]).
-export([create/2, create/3, destroy/2, destroy/3]).
-export([cgroup/1, cgroup/2, fold/5, fold/6, fold_files/6, fold_files/7]).
-export([get/5, set/6]).
-export([is_file/3, is_dir/3]).
-export([mounts/1, mounts/2]).
-export([join/2,relpath/1,expand/1]).

-spec supported(alcove_drv:ref(),alcove:fork_path()) -> boolean().
supported(Drv, Pids) ->
    foreach([
        % running on linux?
        fun() -> {unix,linux} =:= os:type() end,
        % cgroups supported?
        fun() -> is_file(Drv, Pids, "/proc/cgroups") end,
        % cgroups mounted?
        fun() ->
            Exists = [ alcove_cgroup:is_dir(Drv, Pids, Cgroup)
                || Cgroup <- proplists:get_keys(cgroup(Drv, Pids)) ],
            [] =/= Exists andalso true =/= lists:member(false, Exists)
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

-spec create(alcove_drv:ref(),alcove:fork_path()) -> 'ok'.
create(Drv, Pids) ->
    create(Drv, Pids, [<<"alcove">>]).

-spec create(alcove_drv:ref(),alcove:fork_path(),[file:name_all()]) -> 'ok'.
create(Drv, Pids, Namespaces) ->
    [ create_1(Drv, Pids, Namespace) || Namespace <- expand(Namespaces) ],
    ok.

create_1(Drv, Pids, Namespace) ->
    Fun = fun(Cgroup, _Acc) ->
            Path = join(Cgroup, Namespace),
            case alcove:mkdir(Drv, Pids, Path, 8#755) of
                ok -> ok;
                {error,eexist} -> ok;
                Error -> Error
            end
    end,
    fold(Drv, <<>>, [], Fun, []).

-spec destroy(alcove_drv:ref(),alcove:fork_path()) -> [ok | {error, file:posix()}].
destroy(Drv, Pids) ->
    destroy(Drv, Pids, [<<"alcove">>]).

-spec destroy(alcove_drv:ref(),alcove:fork_path(),[file:name_all()]) -> [ok | {error, file:posix()}].
destroy(Drv, Pids, Namespace) ->
    Fun = fun(Cgroup, _Acc) ->
            Path = join(Cgroup, Namespace),
            case alcove:rmdir(Drv, Pids, Path) of
                ok -> ok;
                {error,enoent} -> ok;
                Error -> Error
            end
    end,
    fold(Drv, <<>>, [], Fun, []).

-spec set(alcove_drv:ref(),alcove:fork_path(),binary(),
    [binary()],string() | binary(),string() | binary()) -> ok | {error, file:posix()}.
set(Drv, Pids, MntOpt, Namespace, Key, Value) ->
    Fun = fun(Cgroup, _Acc) ->
            File = join(Cgroup, [Key]),
            write(Drv, Pids, File, Value)
    end,
    case fold(Drv, MntOpt, Namespace, Fun, []) of
        [] -> {error,enoent};
        N -> N
    end.

-spec get(alcove_drv:ref(),alcove:fork_path(),
    binary(),[binary()],string() | binary()) -> {ok,binary()}.
get(Drv, Pids, MntOpt, Namespace, Key) ->
    Fun = fun(Cgroup, _Acc) ->
            File = join(Cgroup, [Key]),
            read(Drv, Pids, File)
    end,
    fold(Drv, MntOpt, Namespace, Fun, []).

-spec write(alcove_drv:ref(),alcove:fork_path(),file:name_all(),iodata()) ->
    {'error',file:posix()} | {'ok',non_neg_integer()}.
write(Drv, Pids, File, Bytes) ->
    Reply = case alcove:open(Drv, Pids, File, [o_wronly], 0) of
        {ok, FH} ->
            Size = iolist_size(Bytes),
            N = alcove:write(Drv, Pids, FH, Bytes),
            alcove:close(Drv, Pids, FH),
            % XXX will crash in the case of a partial write
            case N of
                {ok, Size} ->
                    ok;
                {error, _} = Error ->
                    Error
            end;
        Error ->
            Error
    end,
    Reply.

-spec read(alcove_drv:ref(),alcove:fork_path(),file:name_all()) ->
    {'error',file:posix()} | {'ok',binary()}.
read(Drv, Pids, File) ->
    Reply = case alcove:open(Drv, Pids, File, [o_rdonly], 0) of
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
    case alcove:open(Drv, Pids, "/proc/mounts", [o_rdonly], 0) of
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
    case alcove:open(Drv, Pids, Path, [o_wronly], 0) of
        {error,eisdir} -> true;
        {error,_} -> false;
        {ok, FH} ->
            alcove:close(Drv, Pids, FH),
            false
    end.

is_file(Drv, Pids, File) ->
    case alcove:open(Drv, Pids, File, [o_rdonly], 0) of
        {ok, FH} ->
            alcove:close(Drv, Pids, FH),
            true;
        _ ->
            false
    end.

join(Cgroup, Path) ->
    filename:join([Cgroup|relpath(Path)]).

relpath(Path) ->
    lists:flatmap(fun(N) -> case filename:split(maybe_binary(N)) of
                        [Root|Rest] when Root =:= "/"; Root =:= <<"/">> ->
                            Rest;
                        Rest ->
                            Rest
                    end
          end, Path).

foreach([]) ->
    true;
foreach([Fun|Funs]) ->
    case Fun() of
        true -> foreach(Funs);
        false -> false
    end.

% [<<"a">>, <<"b">>, <<"c">>] ->
%  [[<<"a">>], [<<"a">>,<<"b">>], [<<"a">>,<<"b">>,<<"c">>]]
expand(Path) ->
    [ element(1, lists:split(X, Path)) || X <- lists:seq(1, length(Path)) ].

maybe_binary(N) when is_binary(N) -> N;
maybe_binary(N) when is_list(N) -> list_to_binary(N).
