@doc Convert ioctl constant to integer

== Examples ==

```
1> {ok, Drv} = alcove_drv:start().
{ok,<0.177.0>}
2> alcove:ioctl_constant(Drv, [], siocgifaddr).
35093
'''
