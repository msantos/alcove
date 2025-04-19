@doc setrlimit(2): set a resource limit

Note on `rlimit_nofile':

The control process requires a fixed number of file descriptors for
each subprocess. Reducing the number of file descriptors will reduce
the limit on child processes.

If the file descriptor limit is below the number of file descriptors
currently used, setrlimit/4,5 will return `{error, einval}'.

== Examples ==

```
1> {ok, Drv} = alcove_drv:start().
{ok,<0.177.0>}
2> rr(alcove).
[alcove_jail,alcove_pid,alcove_rlimit,alcove_timeval]
3> {ok, Pid} = alcove:fork(Drv, []).
{ok,28493}
4> alcove:getrlimit(Drv, [Pid], rlimit_nofile).
{ok,#alcove_rlimit{cur = 1024,max = 1048576}}
5> alcove:setrlimit(Drv, [Pid], rlimit_nofile, #alcove_rlimit{cur = 64, max = 64}).
ok
6> alcove:getrlimit(Drv, [Pid], rlimit_nofile).
{ok,#alcove_rlimit{cur = 64,max = 64}}
'''
