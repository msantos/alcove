@doc clone(2): create a new process

== Support ==

* Linux

== Examples ==

```
1> {ok, Drv} = alcove_drv:start([{exec, "sudo -n"}]).
{ok,<0.176.0>}
2> {ok, Pid} = alcove:clone(Drv, [], [clone_newns, clone_newpid, clone_newipc, clone_newuts, clone_newnet]).
{ok,19127}
3> alcove:getpid(Drv, [Pid]).
1
'''
