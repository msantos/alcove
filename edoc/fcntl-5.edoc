@doc fcntl(2): perform operations on a file descriptor

== Examples ==

```
1> {ok, Drv} = alcove_drv:start([{exec, "sudo -n"}]).
{ok,<0.176.0>}
2> {ok, Pid} = alcove:fork(Drv, []).
{ok,31012}

% stdin = 0
3> alcove:fcntl(Drv, [Pid], 0, f_getfd, 0).
{ok,0}
'''
