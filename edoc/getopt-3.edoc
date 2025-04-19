@doc Retrieve port options for event loop

Options are configurable per process, with the default settings inherited
from the parent.

The initial values for these options are set for the port by
alcove_drv:start/1.

* maxchild : non_neg_integer() : 64

  Number of child processes allowed for this control process. The value
  can be modified using setopt/4,5. Additionally, reducing RLIMIT_NOFILE
  for the process may result in a reduced maxchild value.

* exit_status : 1 | 0 : 1

  Controls whether the controlling Erlang process is informed of a
  process exit value.

* maxforkdepth : non_neg_integer() : 16

  Sets the maximum length of the alcove process pipeline.

* termsig : 1 | 0 : 1

  If a child process exits because of a signal, notify the controlling
  Erlang process.

* flowcontrol : int32_t() : -1 (disabled)

  Sets the default flow control behaviour for a newly forked process. Flow
  control is applied after the child process calls exec().

  See setcpid/5.

* signaloneof : 0-255 : 15

  Send a signal to a child process on shutdown (stdin of the alcove
  control process is closed).

  See setcpid/5.

== Examples ==

```
1> {ok, Drv} = alcove_drv:start().
{ok,<0.177.0>}
2> alcove:getopt(Drv, [], maxforkdepth).
16
'''
