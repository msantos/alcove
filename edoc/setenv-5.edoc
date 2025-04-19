@doc setenv(3): set an environment variable

== Examples ==

```
1> {ok, Drv} = alcove_drv:start().
{ok,<0.177.0>}
2> alcove:setenv(Drv, [], "ALCOVE_TEST", "foo", 0).
ok
3> alcove:getenv(Drv, [], "ALCOVE_TEST").
<<"foo">>
4> alcove:setenv(Drv, [], "ALCOVE_TEST", "bar", 0).
ok
5> alcove:getenv(Drv, [], "ALCOVE_TEST").
<<"foo">>
6> alcove:setenv(Drv, [], "ALCOVE_TEST", "bar", 1).
ok
7> alcove:getenv(Drv, [], "ALCOVE_TEST").
<<"bar">>
'''
