@doc umount(2): unmount a filesystem

On BSD systems, calls unmount(2).

== Examples ==

An example of bind mounting a directory within a linux mount namespace:

```
1> {ok, Drv} = alcove_drv:start([{exec, "sudo -n"}]).
{ok,<0.177.0>}
2> {ok, Pid} = alcove:clone(Drv, [], [clone_newns]).
{ok,10059}
3> alcove:mount(Drv, [Pid], "/tmp", "/mnt", "", [ms_bind, ms_rdonly, ms_noexec], "", "").
ok
4> alcove:umount(Drv, [Pid], "/mnt").
ok
'''
