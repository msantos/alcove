@doc write(2): write to a file descriptor

Writes a buffer to a file descriptor and returns the number of bytes
written.

== Examples ==

```
1> {ok, Drv} = alcove_drv:start().
{ok,<0.177.0>}
2> {ok, FD} = alcove:open(Drv, [], "/tmp/alcove-write-test", [o_wronly,o_creat], 8#644).
{ok,6}
3> alcove:write(Drv, [], FD, <<"test">>).
{ok,4}
'''
