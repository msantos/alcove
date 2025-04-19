@doc unsetenv(3): remove an environment variable

== Examples ==

```
1> {ok, Drv} = alcove_drv:start().
{ok,<0.177.0>}
2> alcove:setenv(Drv, [], "ALCOVE_TEST", "foo", 0).
ok
3> alcove:getenv(Drv, [], "ALCOVE_TEST").
<<"foo">>
4> alcove:unsetenv(Drv, [], "ALCOVE_TEST").
ok
5> alcove:getenv(Drv, [], "ALCOVE_TEST").
false
'''
