alcove
======

[![Package Version](https://img.shields.io/hexpm/v/alcove)](https://hex.pm/packages/alcove)
[![Hex Docs](https://img.shields.io/badge/hex-docs)](https://hexdocs.pm/alcove/)

alcove is:

* a control plane for system processes
* an interface for system programming
* a library for building containerized services

_alcove_ is an external port process (a stand-alone
Unix process that communicates with the Erlang VM using
stdin/stdout). [prx](https://github.com/msantos/prx) is a higher level
library that maps the alcove Unix processes to Erlang processes.

Build
-----

      rebar3 compile

      # to run tests (see "Setting Up Privileges")
      rebar3 do clean, compile, ct

      # Linux: statically link using musl
      sudo apt install musl-dev musl-tools

      # clone the kernel headers somewhere
      cd /path/to/dir
      git clone https://github.com/sabotage-linux/kernel-headers.git

      # then compile
      MUSL_INCLUDE=/path/to/dir ./musl-wrapper rebar3 do clean, compile

      ## Generate code
      make gen

Overview
--------

When alcove is started, it enters an event loop:

```erlang
{ok, Drv} = alcove_drv:start().
```

Similar to a shell, alcove waits for a command. For example, alcove can
be requested to fork(2):

```erlang
{ok, Child1} = alcove:fork(Drv, []).
```

Now there are 2 processes in a parent/child relationship, sitting in
their event loops:

```
beam.smp
  |-erl_child_setup
  |   `-alcove
  |       `-alcove
```

Processes are arranged in a pipeline:

* a pipeline is a list of 0 or more integers representing the process IDs

  By default, pipelines are limited to a length of 16 processes. The
  pipeline length can be increased using getopt/3 up to the system limits.

* unlike in a shell, each successive process in the pipeline is forked
  from the previous process

* like a shell pipeline, the stdout of a process is connected to the
  stdin of the next process in the pipeline using a FIFO

The child process is addressed via the pipeline using a list of PIDs:

```erlang
{ok, Child2} = alcove:fork(Drv, [Child1]),
Child2 = alcove:getpid(Drv, [Child1, Child2]).
```

An empty pipeline refers to the port process:

```erlang
{ok, Child3} = alcove:fork(Drv, []).
```

Finally, we can replace the event loop with a system executable by
calling exec(3):

```erlang
ok = alcove:execvp(Drv, [Child1, Child2], "/bin/cat", ["/bin/cat"]).
```

The process tree now looks like:

```
beam.smp
  |-erl_child_setup
  |   `-alcove
  |       |-alcove
  |       |   `-cat
  |       `-alcove
```

We can interact with the process via stdin, stdout and stderr:

```erlang
alcove:stdin(Drv, [Child1, Child2], "hello process\n"),
[<<"hello process\n">>] = alcove:stdout(Drv, [Child1, Child2]).
```

Setting Up Privileges
---------------------

* sudo

```
sudo visudo -f /etc/sudoers.d/99_alcove
<user> ALL = NOPASSWD: /path/to/alcove/priv/alcove
Defaults!/path/to/alcove/priv/alcove !requiretty
```

When starting alcove, pass in the `exec` option:

```erlang
{ok, Drv} = alcove_drv:start([{exec, "sudo -n"}]).
```

* setuid

```
chown root:root priv/alcove
chmod u+s priv/alcove
```

* Linux: file capabilities

  See capabilities(7) and setcap(8).

Creating a chroot
-----------------

The standard Unix way of sandboxing a process is by doing a chroot(2). The
chroot process involves:

* running as root
* setting process limits
* changing the root directory to limit the process view of the filesystem
* changing to an unprivileged user
* running the sandboxed code

See `examples/chrootex.erl`.

We'll create a chroot using an interface like:

```erlang
-spec sandbox(port(), [iodata()]) -> non_neg_integer().
sandbox(Drv, ["/bin/sh", "-i"]).
```

The function returns the system PID of the child process. This would
create an interactive shell we access through standard I/O.

In order to call chroot(2), the port will need root privileges:

```erlang
{ok, Drv} = alcove_drv:start([{exec, "sudo -n"}]).
```

Following the steps outlined earlier, we want to set some process
limits. In this case, we'll use setrlimit(2):

```erlang
setlimits(Drv, Child) ->
    % Disable creation of files
    ok = alcove:setrlimit(
        Drv,
        [Child],
        rlimit_fsize,
        #alcove_rlimit{cur = 0, max = 0}
    ),

    ok = alcove:setrlimit(
        Drv,
        [Child],
        rlimit_nofile,
        #alcove_rlimit{cur = 0, max = 0}
    ),

    % Limit to one process
    ok = alcove:setrlimit(
        Drv,
        [Child],
        rlimit_nproc,
        #alcove_rlimit{cur = 1, max = 1}
    ).
```

Next we chroot and drop root privileges. We will set the user and group
to a random, high UID/GID that is unlikely to conflict with an existing
system user:

```erlang
chroot(Drv, Child, Path) ->
    ok = alcove:chroot(Drv, [Child], Path),
    ok = alcove:chdir(Drv, [Child], "/").

drop_privs(Drv, Child, Id) ->
    ok = alcove:setgid(Drv, [Child], Id),
    ok = alcove:setuid(Drv, [Child], Id).

id() ->
    16#f0000000 + crypto:rand_uniform(0, 16#ffff).
```

Tying it all together:

```erlang
% The default is to run the cat command. Because of the chroot, we need
% to use a statically linked executable.
sandbox(Drv) ->
    sandbox(Drv, ["/bin/busybox", "cat"]).
sandbox(Drv, Argv) ->
    {Path, Arg0, Args} = argv(Argv),

    {ok, Child} = alcove:fork(Drv, []),

    setlimits(Drv, Child),
    chroot(Drv, Child, Path),
    drop_privs(Drv, Child, id()),

    ok = alcove:execvp(Drv, [Child], Arg0, [Arg0, Args]),

    Child.

% Set the program path for the chroot
argv([Arg0, Args]) ->
    Path = filename:dirname(Arg0),
    Progname = filename:join(["/", filename:basename(Arg0)]),
    {Path, Progname, Args}.
```

Compile and run the example:

```
# If alcove is in ~/src/alcove
export ERL_LIBS=~/src
make eg
rebar shell
```

```erlang
1> {ok, Drv} = chrootex:start().

2> Cat = chrootex:sandbox(Drv).
31831

3> alcove:stdin(Drv, [Cat], "test test\n").
4> alcove:stdout(Drv, [Cat]).
[<<"test test\n">>]
```

We can test the limits of the sandbox by using a shell instead of
herding cats:

```erlang
5> Sh = chrootex:sandbox(Drv, ["/bin/busybox", "sh"]).
31861

% Test the shell is working
6> alcove:stdin(P, [Sh], "echo hello\n").
ok
7> alcove:stdout(P, [Sh]).
[<<"hello\n">>]

% Attempt to create a file
6> alcove:stdin(Drv, [Sh], "> foo\n").
ok
7> alcove:stderr(P, [Sh]).
[<<"sh: can't create foo: Too many open files\n">>]

% Try to fork a new process
8> alcove:stdin(Drv, [Sh], "ls\n").
9> alcove:stderr(P, [Sh]).
[<<"sh: can't fork\n">>]

% If we check the parent for events, we can see the child has exited
10> alcove:event(P, []).
{signal,sigchld}
```

Creating a Container Using Linux Namespaces
-------------------------------------------

Namespaces are the basis for Linux Containers (LXC). Creating a
new namespace is as simple as passing in the appropriate flags to
clone(2). We'll rewrite the chroot example to run inside a namespace and
use another Linux feature, control groups, to limit the system resources
available to the process.

See `examples/nsex.erl`.

* set process limits using cgroups (see cpuset(7))

  When the port is started, we'll create a new cgroup just for our
  application and, whenever a sandboxed process is forked, we'll add it
  to this cgroup.

```erlang
start() ->
    {ok, Drv} = alcove_drv:start([{exec, "sudo -n"}]),

    % Create a new cgroup for our processes
    ok = alcove_cgroup:create(Drv, [], <<"alcove">>),

    % Set the CPUs these processes are allowed to run on. For example,
    % if there are 4 available CPUs, any process in this cgroup will only
    % be able to run on CPU 0
    {ok, 1} = alcove_cgroup:set(
        Drv,
        [],
        <<"cpuset">>,
        <<"alcove">>,
        <<"cpuset.cpus">>,
        <<"0">>
    ),
    {ok, 1} = alcove_cgroup:set(
        Drv,
        [],
        <<"cpuset">>,
        <<"alcove">>,
        <<"cpuset.mems">>,
        <<"0">>
    ),

    % Set the amount of memory available to the process

    % Total memory, including swap. We allow this to fail, because some
    % systems may not have a swap partition/file
    alcove_cgroup:set(
        Drv,
        [],
        <<"memory">>,
        <<"alcove">>,
        <<"memory.memsw.limit_in_bytes">>,
        <<"16m">>
    ),

    % Total memory
    {ok, 3} = alcove_cgroup:set(
        Drv,
        [],
        <<"memory">>,
        <<"alcove">>,
        <<"memory.limit_in_bytes">>,
        <<"16m">>
    ),

    Drv.

setlimits(Drv, Child) ->
    % Add our process to the "alcove" cgroup
    {ok, _} = alcove_cgroup:set(
        Drv,
        [],
        <<>>,
        <<"alcove">>,
        <<"tasks">>,
        integer_to_list(Child)
    ).
```

* running the code involves calling clone(2) to create the namespaces,
  rather than using fork(2)

```erlang
sandbox(Drv, Argv) ->
    {Path, Arg0, Args} = argv(Argv),

    {ok, Child} = alcove:clone(Drv, [], [
        % IPC
        clone_newipc,
        % network
        clone_newnet,
        % mounts
        clone_newns,
        % PID, Child is PID 1 in the namespace
        clone_newpid,
        % hostname
        clone_newuts
    ]),

    setlimits(Drv, Child),
    chroot(Drv, Child, Path),
    drop_privs(Drv, Child, id()),

    ok = alcove:execvp(Drv, [Child], Arg0, [Arg0, Args]),

    Child.
```

Operating System Support
------------------------

Functions marked as operating system specific raise an undefined function
error on unsupported platforms.

Event Loop
----------

These functions can be called while the process is running in the event
loop. Using these functions after the process has called exec(3) will
probably confuse the process.

Functions accepting a constant() will return {error, enotsup} if an
atom is used as the argument and is not found on the platform.

The alcove module functions accept an additional argument which allows
setting timeouts. For example:

    write(Drv, Pipeline, FD, Buf) -> {ok, Count} | {error, posix()}
    write(Drv, Pipeline, FD, Buf, timeout()) -> {ok, Count} | {error, posix()}

By default, timeout is set to infinity. Similar to gen_server:call/3,
setting an integer timeout will cause the process to crash if the
timeout is reached. If the failure is caught, the caller must deal
with any delayed messages that arrive for the Unix process described by
the pipeline.

See "Message Format" for a description of the messages.

Message Format
--------------

* synchronous replies to calls from alcove processes running in the
  event loop. The type of the last element of the tuple depends on the call
  (e.g., open/4,5 would return either {ok, integer()} or {error, posix()}

        {alcove_call, pid(), [non_neg_integer()], term()}

* asynchronous events generated by the alcove process (e.g., signals).

        {alcove_event, pid(), [non_neg_integer()], term()}

* standard error: can be generated by an alcove process running in the
  event loop as well as a Unix process.

        {alcove_stderr, pid(), [non_neg_integer()], binary()}

* standard output: output from the Unix process after alcove has called
  exec(3)

        {alcove_stdout, pid(), [non_neg_integer()], binary()}

Examples
--------

To compile the examples:

```
make eg
```

* GPIO

`examples/gpioled.erl` is a simple example of interacting with the GPIO
on a beaglebone black or raspberry pi that will blink an LED. The example
works with two system processes:

    * a port process which requests the GPIO pin be exported to user
      space, forks a child into a new namespace, then drops privileges

    * a child process gets a file descriptor for the GPIO, then drops
      privileges
