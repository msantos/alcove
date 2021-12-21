@doc setgroups(2): set the supplementary groups of the process

== Examples ==

```
1> {ok, Drv} = alcove_drv:start([{exec, "sudo -n"}]).
{ok,<0.208.0>}
2> {ok, Pid} = alcove:fork(Drv, []).
{ok,44378}
75> alcove:setgroups(Drv, [Pid], []).
ok
76> alcove:getgroups(Drv, [Pid]).
{ok,[]}
'''
