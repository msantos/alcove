@doc chown(2): change file ownership

== Examples ==

```
1> {ok, Drv} = alcove_drv:start([{exec, "sudo -n"}]).
{ok,<0.177.0>}
2> {ok, Pid} = alcove:fork(Drv, []).
{ok,18820}
3> {ok, FD} = alcove:open(Drv, [Pid], "/tmp/foo123.txt", [o_wronly, o_creat], 8#644).
{ok,6}
6> alcove:chown(Drv, [Pid], "/tmp/foo123.txt", 0, 0).
ok
'''
