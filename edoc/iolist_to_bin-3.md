@private
@doc Convert iolist to binary

Test conversion of iolists to binary in a control process.

== Examples ==

```
1> {ok, Drv} = alcove_drv:start().
{ok,<0.177.0>}
2> alcove:iolist_to_bin(Drv, [], ["foo", [<<"1">>, <<"2">>, [<<"3">>], ["bar"]]]).
<<"foo123bar">>
'''
