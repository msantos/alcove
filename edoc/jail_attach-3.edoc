@doc jail_attach(2): join a jailed process

== Support ==

* FreeBSD

== Examples ==

```
1> {ok, Drv} = alcove_drv:start([{exec, "sudo -n"}]).
{ok,<0.208.0>}
2> {ok, Pid} = alcove:fork(Drv, []).
{ok,44378}
3> rr(alcove).
[alcove_jail,alcove_pid,alcove_rlimit,alcove_timeval]
4> Jail0 = alcove_cstruct:jail(#alcove_jail{
4>         path = "/rescue",
4>         hostname = "test0",
4>         jailname = "jail0"
4>     }).
[<<2,0,0,0>>,
 <<0,0,0,0>>,
 {ptr,<<47,114,101,115,99,117,101,0>>},
 {ptr,<<116,101,115,116,48,0>>},
 {ptr,<<106,97,105,108,48,0>>},
 <<0,0,0,0,0,0,0,0>>,
 {ptr,<<>>},
 {ptr,<<>>}]
5> {ok, JID0} = alcove:jail(Drv, [Pid], Jail0).
{ok,21}
6> {ok, Pid0} = alcove:fork(Drv, []).
{ok,44379}
7> ok = alcove:jail_attach(Drv, [Pid0], JID0).
ok
8> alcove:gethostname(Drv, [Pid0]).
{ok,<<"test0">>}
9> ok = alcove:jail_remove(Drv, [], JID0).
ok
'''
