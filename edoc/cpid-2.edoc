@doc Returns the list of child PIDs for this process

== Examples ==

```
1> {ok, Drv} = alcove_drv:start().
{ok,<0.176.0>}
2> {ok, Pid1} = alcove:fork(Drv, []).
{ok,19048}
3> {ok, Pid2} = alcove:fork(Drv, []).
{ok,19127}
4> rr(alcove).
[alcove_jail,alcove_pid,alcove_rlimit,alcove_timeval]
5> alcove:cpid(Drv, [Pid1]).
[]
6> alcove:cpid(Drv, []).
[#alcove_pid{pid = 19048,flowcontrol = -1,signaloneof = 15,
             fdctl = 7,stdin = 9,stdout = 10,stderr = 12},
 #alcove_pid{pid = 19127,flowcontrol = -1,signaloneof = 15,
             fdctl = 8,stdin = 13,stdout = 14,stderr = 16}]
'''
