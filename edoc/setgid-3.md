@doc setgid(2): set the GID of the process

== Examples ==

```
1> {ok, Drv} = alcove_drv:start([{exec, "sudo -n"}]).
{ok,<0.208.0>}
2> {ok, Pid} = alcove:fork(Drv, []).
{ok,44378}
72> alcove:setgid(Drv, [Pid], 123).
ok
73> alcove:getgid(Drv, [Pid]).
123
'''
