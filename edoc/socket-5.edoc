@doc socket(2): returns a file descriptor for a communication endpoint

== Examples ==

```
1> {ok, Drv} = alcove_drv:start().
{ok,<0.177.0>}
2> {ok, Socket} = alcove:socket(Drv, [], af_inet, sock_stream, 0).
{ok,6}
3> alcove:close(Drv, [], Socket).
ok
'''
