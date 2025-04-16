@doc Set options for child process of alcove control process

`flowcontrol' enables rate limiting of the stdout and stderr of a child
process. stdin is not rate limited (default: -1 (disabled))

* 0: stdout/stderr for process is not read

* 1-2147483646: read this many messages from the process

* -1: disable flow control

NOTE: the limit applies to stdout and stderr. If the limit is set to 1,
it is possible to get:

* 1 message from stdout

* 1 message from stderr

* 1 message from stdout and stderr

`signaloneof' delivers a signal to any subprocesses when the alcove
control process shuts down (default: 15 (SIGTERM))

== Examples ==

```
1> {ok, Drv} = alcove_drv:start().
{ok,<0.177.0>}
2> {ok, Pid} = alcove:fork(Drv, []).
{ok,24440}
3> alcove:setcpid(Drv, [], Pid, flowcontrol, 0).
true
4> alcove:getcpid(Drv, [], Pid, flowcontrol).
0
'''
