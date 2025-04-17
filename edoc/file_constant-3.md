@doc Constants for open(2)

== Examples ==

```
1> {ok, Drv} = alcove_drv:start().
{ok,<0.177.0>}
2> alcove:file_constant(Drv, [], o_rdonly).
0
'''
