@doc chmod(2): change file permissions

== Examples ==

```
1> {ok, Drv} = alcove_drv:start().
{ok,<0.177.0>}
2> {ok, Pid} = alcove:fork(Drv, []).
{ok,18820}
3> {ok, FD} = alcove:open(Drv, [Pid], "/tmp/foo123.txt", [o_wronly, o_creat], 8#644).
{ok,6}
4> alcove:chmod(Drv, [Pid], "/tmp/foo123.txt", 8#400).
ok
'''
