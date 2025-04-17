@doc mkdir(2): create a directory

== Examples ==

```
1> {ok, Drv} = alcove_drv:start().
{ok,<0.177.0>}
2> alcove:mkdir(Drv, [], "/tmp/alcove-mkdir-test", 8#755).
ok
'''
