@doc link(2) : create a hard link

== Examples ==

```
1> {ok, Drv} = alcove_drv:start().
{ok,<0.177.0>}
2> {ok, Pid} = alcove:fork(Drv, []).
{ok,18820}
3> {ok, FD} = alcove:open(Drv, [Pid], "/tmp/alcove-link-test.txt", [o_wronly, o_creat], 8#644).
{ok,6}
4> alcove:link(Drv, [Pid], "/tmp/alcove-link-test.txt", "/tmp/alcove-link-test.link").
ok
'''
