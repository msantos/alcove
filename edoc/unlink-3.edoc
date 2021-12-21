@doc unlink(2): delete a name from the filesystem

== Examples ==

```
1> {ok, Drv} = alcove_drv:start().
{ok,<0.177.0>}
2> alcove:open(Drv, [], "/tmp/alcove-open-test", [o_wronly,o_creat], 8#644).
{ok,6}
3> alcove:unlink(Drv, [], "/tmp/alcove-open-test").
ok
'''
