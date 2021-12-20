@doc read(2): read bytes from a file descriptor

== Examples ==

```
1> {ok, Drv} = alcove_drv:start().
{ok,<0.209.0>}
2> {ok, Pid} = alcove:fork(Drv, []).
{ok,75710}
3> {ok, FD} = alcove:open(Drv, [Pid], "/etc/passwd", [o_rdonly], 0).
{ok,6}
4> alcove:read(Drv, [Pid], FD, 64).
{ok,<<"# $FreeBSD$\n#\nroot:*:0:0:Charlie &:/root:/bin/csh\ntoor:*:0:0:Bou">>}
'''
