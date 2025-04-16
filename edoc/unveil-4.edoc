@doc unveil(2): restrict filesystem view

To disable unveil calls, use an empty list ([]) or, equivalently, an
empty string ("").

```
alcove:unveil(Drv, [Task], <<"/etc">>, <<"r">>),
alcove:unveil(Drv, [Task], [], []).
'''

== Support ==

* OpenBSD

== Examples ==

```
1> {ok, Drv} = alcove_drv:start().
{ok,<0.177.0>}
2> {ok, Pid} = alcove:fork(Drv, []).
{ok,28978}
3> alcove:unveil(Drv, [Pid], <<"/etc">>, <<"r">>).
ok
4> alcove:unveil(Drv, [Pid], [], []).
ok
5> alcove:readdir(Drv, [Pid], "/etc").
{ok,[<<".">>,<<"..">>,<<"acme">>,<<"amd">>,<<"authpf">>,
     <<"daily">>,<<"disktab">>,<<"examples">>,<<"firmware">>,
     <<"hotplug">>,<<"iked">>,<<"isakmpd">>,<<"ldap">>,
     <<"magic">>,<<"mail">>,<<"moduli">>,<<"monthly">>,
     <<"mtree">>,<<"netstart">>,<<"npppd">>,<<"pf.os">>,
     <<"ppp">>,<<"protocols">>,<<"rc">>,<<"rc.conf">>,<<"rc.d">>,
     <<...>>|...]}
6> alcove:readdir(Drv, [Pid], "/tmp").
{error,enoent}
'''
