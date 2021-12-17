@doc execvp(2): replace the current process image using the search path

== Examples ==

```
1> {ok, Drv} = alcove_drv:start().
{ok,<0.176.0>}
2> {ok, Pid} = alcove:fork(Drv, []).
{ok,29087}
3> alcove:execvp(Drv, [Pid], "cat", ["cat"]).
ok
4> alcove:stdin(Drv, [Pid], "test\n").
ok
5> alcove:stdout(Drv, [Pid]).
[<<"test\n">>]
6> alcove:stdin(Drv, [Pid], "123\n").
ok
7> flush().
Shell got {alcove_stdout,<0.176.0>,[29087],<<"123\n">>}
ok
'''
