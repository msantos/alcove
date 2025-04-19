@doc connect(2): initiate a connection on a socket

== Examples ==

```
*module(unix_socket).

*export([connect]).

connect(Data) when is_binary(Data) ->
    {ok, Drv} = alcove:start(),
    {ok, NC} = alcove:fork(Drv, []),
    {ok, Process} = alcove:fork(Drv, []),

    Sockname = <<"/tmp/test.", (integer_to_binary(alcove:getpid(Drv, [])))/binary>>,
    ok = alcove:execvp(Drv, [NC], "nc", ["nc", "-l", "-U", Sockname]),

    ok = waitfor(Sockname),

    {ok, Socket} = alcove:socket(Drv, [Process], af_unix, sock_stream, 0),

    % #define UNIX_PATH_MAX   108
    % struct sockaddr_un {
    % 	__kernel_sa_family_t sun_family; /* AF_UNIX */
    % 	char sun_path[UNIX_PATH_MAX];   /* pathname */
    % };
    AF_UNIX = 1,
    SocknameLen = byte_size(Sockname),
    Len = (unix_path_max() - SocknameLen) * 8,
    ok = alcove:connect(Drv, [Process], Socket, [
        sockaddr_common(AF_UNIX, SocknameLen),
        Sockname,
        <<0:Len>>
    ]),

    % alcove process -> nc
    {ok, N} = alcove:write(Drv, [Process], Socket, Data),
    receive
        {alcove_stdout, Drv, [NC], Stdout} ->
            Stdout
    end.

% UNIX_PATH_MAX
unix_path_max() ->
    case erlang:system_info(os_type) of
        {unix, BSD} when BSD == darwin; BSD == openbsd; BSD == netbsd; BSD == freebsd ->
            104;
        {unix, _} ->
            108
    end.

% struct sockaddr
sockaddr_common(Family, Length) ->
    case erlang:system_info(os_type) of
        {unix, BSD} when BSD == darwin; BSD == openbsd; BSD == netbsd; BSD == freebsd ->
            <<Length:8, Family:8>>;
        {unix, _} ->
            <<Family:16/native>>
    end.

waitfor(Sockname) ->
    case file:read_file_info(Sockname) of
        {ok, _} ->
            ok;
        {error, enoent} ->
            timer:sleep(1),
            waitfor(Sockname);
        {error, eperm} ->
            ok;
        Error ->
            Error
    end.
'''
