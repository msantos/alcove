@doc Retrieve the alcove version

== Examples ==

```
1> {ok, Drv} = alcove_drv:start().
{ok,<0.177.0>}
2> alcove:version(Drv, []).
<<"0.37.0">>
'''
