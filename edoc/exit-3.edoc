@doc exit(3): cause an alcove control process to exit

== Examples ==

```
1> {ok, Drv} = alcove_drv:start([{exec, "sudo -n"}]).
{ok,<0.176.0>}
2> {ok, Pid} = alcove:fork(Drv, []).
{ok,30973}
3> alcove:exit(Drv, [Pid], 111).
ok
4> flush().
Shell got {alcove_event,<0.176.0>,[30973],{exit_status,111}}
ok
'''
