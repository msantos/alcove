@doc unshare(2): create a new namespace in the current process

Make a new namespace without calling clone(2):

```
ok = alcove:unshare(Drv, [], [clone_newnet]).
% The port is now running in a namespace without network access.
'''

== Examples ==

```
1> {ok, Drv} = alcove_drv:start([{exec, "sudo -n"}]).
{ok,<0.177.0>}
2> {ok, Pid} = alcove:fork(Drv, []).
{ok,28493}
3> alcove:unshare(Drv, [Pid], [clone_newuts]).
ok
4> alcove:sethostname(Drv, [Pid], "unshare").
ok
5> alcove:gethostname(Drv, [Pid]).
{ok,<<"unshare">>}
6> alcove:gethostname(Drv, []).
{ok,<<"host1">>}
'''
