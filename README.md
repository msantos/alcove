alcove
------

alcove is an Erlang interface for creating application containers like
sandboxes or Linux containers.

alcove acts like a sort of shell supporting programmatic access to system
primitives useful for isolating and interacting with Unix processes.

Overview
========

When alcove is started, it enters an event loop:

```erlang
{ok, Drv} = alcove_drv:start().
```

Much like a shell, alcove waits for a command. For example, alcove can
be requested to fork(2):

```erlang
{ok, Child1} = alcove:fork(Drv).
```

Now there are 2 processes in a parent/child relationship, sitting in
their event loops. We access the child process by using the fork path:

```erlang
{ok, Child2} = alcove:fork(Drv, [Child1]),
Child2 = alcove:getpid(Drv, [Child1,Child2]).
```

An empty fork path refers to the port process:

```erlang
% Same as doing: alcove:fork(Drv).
{ok, Child3} = alcove:fork(Drv, []).
```

Fork paths can be arbitrarily long (well, until you hit a system limit
or overflow the stack) but, by default, are limited to a length of 16
Unix processes.

Finally, we can replace the event loop with a system executable by
calling exec(3):

```erlang
ok = alcove:execvp(Drv, [Child1,Child2], "/bin/cat", ["/bin/cat"]).
```

We can interact with the process via stdin, stdout and stderr:

```erlang
alcove:stdin(Drv, [Child1,Child2], "hello process\n"),
<<"hello process\n">> = alcove:stdout(Drv, [Child1,Child2]).
```

Setting Up Privileges
=====================

* sudo

```
sudo visudo -f /etc/sudoers.d/99_alcove
<user> ALL = NOPASSWD: /path/to/alcove/priv/alcove
Defaults!/path/to/alcove/priv/alcove !requiretty
```

When starting alcove, pass in the `exec` option:

```erlang
{ok, Drv} = alcove_drv:start([{exec, "sudo"}]).
```

* setuid

```
chown root:root priv/alcove
chmod u+s priv/alcove
```

* Linux: file capabilites

  See capabilities(7) and setcap(8).

Creating a chroot
=================

The standard Unix way of sandboxing a process is by doing a chroot(2). The
process usually involves:

* running as root
* setting process limits
* changing the root directory to limit the process' view of the filesystem
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
{ok, Drv} = alocve_drv:start([{exec, "sudo"}]).
```

Following the steps outlined earlier, we want to set some process
limits. In this case, we'll use setrlimit(2):

```erlang
setlimits(Drv, Child) ->
    % Convert atoms to integers defined on this platform
    RLIMIT_FSIZE = alcove:rlimit_define(Drv, rlimit_fsize),
    RLIMIT_NOFILE = alcove:rlimit_define(Drv, rlimit_nofile),
    RLIMIT_NPROC = alcove:rlimit_define(Drv, rlimit_nproc),

    % Disable creation of files
    ok = alcove:setrlimit(Drv, [Child], RLIMIT_FSIZE,
            #alcove_rlimit{cur = 0, max = 0}),

    ok = alcove:setrlimit(Drv, [Child], RLIMIT_NOFILE,
            #alcove_rlimit{cur = 0, max = 0}),

    % Limit to one process
    ok = alcove:setrlimit(Drv, [Child], RLIMIT_NPROC,
            #alcove_rlimit{cur = 1, max = 1}).
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

    {ok, Child} = alcove:fork(Drv),

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
./start.sh
```

```erlang
1> {ok, Drv} = chrootex:start().

2> Cat = chrootex:sandbox(Drv).
31831

3> alcove:stdin(Drv, [Cat], "test test\n").
4> alcove:stdout(Drv, [Cat]).
<<"test test\n">>
```

We can test the limits of the sandbox by using a shell instead of
herding cat's:

```erlang
5> Sh = chrootex:sandbox(Drv, ["/bin/busybox", "sh"]).
31861

% Test the shell is working
6> alcove:stdin(P, [31861], "echo hello\n").
true
7> alcove:stdout(P, [31861]).
<<"hello\n">>

% Attempt to create a file
6> alcove:stdin(Drv, [Sh], "> foo\n").
true
7> alcove:stderr(P, [31861]).
<<"sh: can't create foo: Too many open files\n">>

% Try to fork a new process
8> alcove:stderr(P, [31861]).
<<"sh: can't fork\n">>

% If we check the parent for events, we can see the child has exited
10> alcove:event(P).
{signal,sigchld}
```

Creating a Container Using Linux Namespaces
===========================================

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
    {ok, Drv} = alcove_drv:start([{exec, "sudo"}]),

    % Create a new cgroup for our processes
    ok = alcove_cgroup:create(Drv, [], <<"alcove">>),

    % Set the CPUs these processes are allowed to run on. For example,
    % if there are 4 available CPUs, any process in this cgroup will only
    % be able to run on CPU 0
    {ok,1} = alcove_cgroup:set(Drv, [], <<"cpuset">>, <<"alcove">>,
            <<"cpuset.cpus">>, <<"0">>),
    {ok,1} = alcove_cgroup:set(Drv, [], <<"cpuset">>, <<"alcove">>,
            <<"cpuset.mems">>, <<"0">>),

    % Set the amount of memory available to the process

    % Total memory, including swap. We allow this to fail, because some
    % systems may not have swap set up
    alcove_cgroup:set(Drv, [], <<"memory">>, <<"alcove">>,
            <<"memory.memsw.limit_in_bytes">>, <<"16m">>),

    % Total memory
    {ok,3} = alcove_cgroup:set(Drv, [], <<"memory">>, <<"alcove">>,
            <<"memory.limit_in_bytes">>, <<"16m">>),

    Drv.

setlimits(Drv, Child) ->
    % Add our process to the "alcove" cgroup
    {ok,_} = alcove_cgroup:set(Drv, [], <<>>, <<"alcove">>,
            <<"tasks">>, integer_to_list(Child)).

```

* running the code involves calling clone(2) to create the namespaces,
  rather than using fork(2)

```erlang
sandbox(Drv, Argv) ->
    {Path, Arg0, Args} = argv(Argv),

    Flags = alcove:define(Drv, [
            clone_newipc, % IPC
            clone_newnet, % network
            clone_newns,  % mounts
            clone_newpid, % PID, Child is PID 1 in the namespace
            clone_newuts  % hostname
            ]),

    {ok, Child} = alcove:clone(Drv, Flags),

    setlimits(Drv, Child),
    chroot(Drv, Child, Path),
    drop_privs(Drv, Child, id()),

    ok = alcove:execvp(Drv, [Child], Arg0, [Arg0, Args]),

    Child.
```

alcove_drv
==========

    start() -> port()
    start(Options) -> port()

    Types   Options = [Option]
            Option = stderr_to_stdout | {env, [{Key, Val}]}
                | {exec, string()}
                | {progname, string()}
                | {exit_status, boolean()}
                | {termsig, boolean()}
                | verbose | {verbose, non_neg_integer()}
                | {maxchild, non_neg_integer()}
                | {maxforkdepth, non_neg_integer()}

    Create the alcove port.

        stderr_to_stdout

            The behaviour of stderr from the port differs from child
            processes. Standard error from the port goes to the console
            while stderr from child processes is tagged and sent to the
            controlling Erlang process.

            This option merges stderr and stdout from the port. Since
            stdout is used for communicating with the Erlang side and
            is tagged with a header, this will likely mess things up.

            Only use this option if you want to call execvp/3,4 in
            the port.

        {env, [{Key,Val}]}

            Set the environment for the port.

        {exec, Exec}

            Default: ""

            Sets a command to run the port, such as sudo.

        {progname, Path}

            Default: priv/alcove

            Sets the path to the alcove executable.

    For the remaining options, see alcove:getopt/2,3.

alcove
======

If any of these functions do not include the fork path, the call is
evaluated by the port. The following functions are equivalent:

    alcove:getpid(Drv)
    alcove:getpid(Drv, [])

    alcove:chown(Drv, "/tmp/test", 8#600)
    alcove:chown(Drv, [], "/tmp/test", 8#600)

Functions marked as operating system specific will return
{error,unsupported} on other platforms.

### Data Types

    Drv = pid()
    Pid = non_neg_integer()
    Pids = [Pid]
    Path = iodata()
    FD = integer()

### Event Loop

These functions can be called while the process is running in the event
loop. Using these functions after the process has called exec(3) will
probably confuse the process.

    chdir(Drv, Path) -> ok | {error, posix()}
    chdir(Drv, Pids, Path) -> ok | {error, posix()}

        chdir(2) : change process current working directory.

    chmod(Drv, Path, Mode) -> ok | {error, posix()}
    chmod(Drv, Pids, Path, Mode) -> ok | {error, posix()}

        chmod(2) : change file permissions

    chown(Drv, Path, Owner, Group) -> ok | {error, posix()}
    chown(Drv, Pids, Path, Owner, Group) -> ok | {error, posix()}

        Types   Owner = Group = non_neg_integer()

        chown(2) : change file ownership

    chroot(Drv, Path) -> ok | {error, posix()}
    chroot(Drv, Pids, Path) -> ok | {error, posix()}

        chroot(2) : change root directory

    clearenv(Drv) -> ok | {error, posix()}
    clearenv(Drv, Pids) -> ok | {error, posix()}

        clearenv(3) : zero process environment

    clone(Drv, Flags) -> {ok, integer()} | {error, posix()}
    clone(Drv, Pids, Flags) -> {ok, integer()} | {error, posix()}

        Types   Flags = integer()

        Linux only.

        clone(2) : create a new process

    clone_define(Drv, atom()) -> integer() | unknown
    clone_define(Drv, Pids, atom()) -> integer() | unknown

        Linux only.

        Map symbols to integer constants.

    close(Drv, FD) -> ok | {error, posix()}
    close(Drv, Pids, FD) -> ok | {error, posix()}

        close(2) : close a file descriptor.

    environ(Drv) -> [binary()]
    environ(Drv, Pids) -> [binary()]

        environ(7) : return the process environment variables

    event(Drv) -> term()
    event(Drv, Pids) -> term()

        event/1,2 is used to retrieve async messages returned from the
        port, such as trapped signals, the exit status or the termination
        signal.

    execve(Drv, Arg0, [Arg0, Args], Env) -> ok | {error, posix()}
    execve(Drv, Pids, Arg0, [Arg0, Args], Env) -> ok | {error, posix()}

        Types   Arg0 = Args = iodata()
                Env = [iodata()]

        execve(2) : replace the process image, specifying the environment
        for the new process image.

    execvp(Drv, Arg0, [Arg0, Args]) -> ok | {error, posix()}
    execvp(Drv, Pids, Arg0, [Arg0, Args]) -> ok | {error, posix()}

        Types   Arg0 = Args = iodata()
                Env = [iodata()]

        execvp(2) : replace the current process image using the search path

    exit(Drv, Value) -> ok
    exit(Drv, Pids, Value) -> ok

        Types   Value = integer()

        exit(3) : cause the child process to exit

    file_define(Drv, atom()) -> integer() | unknown
    file_define(Drv, Pids, atom()) -> integer() | unknown

        Constants for open(2).

    fork(Drv) -> {ok, integer()} | {error, posix()}
    fork(Drv, Pids) -> {ok, integer()} | {error, posix()}

        fork(2) : create a new process

    getcwd(Drv) -> {ok, binary()} | {error, posix()}
    getcwd(Drv, Pids) -> {ok, binary()} | {error, posix()}

        getcwd(3) : return the current working directory

    getenv(Drv, iodata()) -> binary() | false
    getenv(Drv, Pids, iodata()) -> binary() | false

        getenv(3) : retrieve an environment variable

    getgid(Drv) -> integer()
    getgid(Drv, Pids) -> integer()

        getgid(2) : retrieve the processes' group ID

    gethostname(Drv) -> {ok, binary()} | {error, posix()}
    gethostname(Drv, Pids) -> {ok, binary()} | {error, posix()}

        gethostname(2) : retrieve the system hostname

    getopt(Drv, Options) -> integer() | false
    getopt(Drv, Pids, Options) -> integer() | false

        Types   Options = verbose | childlimit | exit_status | maxchild |
                          maxforkdepth | termsig

        Retrieve port options for event loop. These options are
        configurable per process, with the default settings inherited
        from the parent.

        The initial values for these options are set for the port by
        alcove_drv:start/1.

            verbose : non_neg_integer() : 0

                Write debug information to stderr. The port will send
                stderr to the console.

            childlimit : non_neg_integer() : (ulimit -n) / 4 - 4

                Number of child processes allowed for this process. This
                value cannot be changed by a running process.

            exit_status : 1 | 0 : 0

                Controls whether the controlling Erlang process is
                informed of a process' exit value.

            maxchild : non_neg_integer() : childlimit

                Sets the childlimit for processes forked by this process.

                This value can be greater than the running process'
                childlimit.

            maxforkdepth : non_neg_integer() : 16

                Sets the maximum length of the fork path.

            termsig : 1 | 0 : 0

                If a child process exits because of a signal, notify
                the controlling Erlang process.

    getpgrp(Drv) -> integer()
    getpgrp(Drv, Pids) -> integer()

        getpgrp(2) : retrieve the process group.

    getpid(Drv) -> integer()
    getpid(Drv, Pids) -> integer()

        getpid(2) : retrieve the system PID of the process.

    getrlimit(Drv, integer() -> {ok, #alcove_rlimit{}} | {error, posix()}
    getrlimit(Drv, Pids, integer() -> {ok, #alcove_rlimit{}} | {error, posix()}

        getrlimit(2) : retrive the resource limits for a process. Returns
        a record:

            -include_lib("alcove/include/alcove.hrl").

            #alcove_rlimit{
                cur = integer(),
                max = integer()
                }

    getsid(Drv, Pid) -> {ok, integer()} | {error, posix()}
    getsid(Drv, Pids, Pid) -> {ok, integer()} | {error, posix()}

        getsid(2) : retrieve the session ID

    getuid(Drv) -> integer()
    getuid(Drv, Pids) -> integer()

        getuid(2) : returns the process user ID

    kill(Drv, Pid, Signal) -> ok | {error, posix()}
    kill(Drv, Pids, Pid, Signal) -> ok | {error, posix()}

        Types   Signal = integer()

        kill(2) : terminate a process

    lseek(Drv, FD, Offset, Whence) -> ok | {error, posix()}
    lseek(Drv, Pids, FD, Offset, Whence) -> ok | {error, posix()}

        Types   Offset = Whence = integer()

        lseek(2) : set file offset for read/write

    mkdir(Drv, Path, Mode) -> ok | {error, posix()}
    mkdir(Drv, Pids, Path, Mode) -> ok | {error, posix()}

        mkdir(2) : create a directory

    mount(Drv, Source, Target, FSType, Flags, Data) -> ok | {error, posix()}
    mount(Drv, Pids, Source, Target, FSType, Flags, Data) -> ok
        | {error, posix()}

        Types   Source = Target = FSType = Data = iodata()
                Flags = integer()

        mount(2) : mount a filesystem, Linux style

        On BSD systems, the Source argument is ignored and passed to
        the system mount call as:

            mount(FSType, Target, Flags, Data);

    mount_define(Drv, Flag) -> integer() | unknown
    mount_define(Drv, Pids, Flag) -> integer() | unknown

        Types   Flag = rdonly | nosuid | noexec | noatime | ...

        Convert flag names to integers. The lower case atoms are used
        for portability:

            alcove:mount_define(Drv, rdonly)

        'rdonly' is mapped to MS_RDONLY on Linux and MNT_RDONLY on
        FreeBSD.

    open(Drv, Path, Flags, Mode) -> {ok, integer()} | {error, posix()}
    open(Drv, Pids, Path, Flags, Mode) -> {ok, integer()} | {error, posix()}

        Types   Flags = Mode = integer()

        open(2) : returns a file descriptor associated with a file

    pid(Drv) -> [Pid]
    pid(Drv, Pids) -> [Pid]

        Returns the list of child PIDs for this process.

    prctl(Drv, Option, Arg2, Arg3, Arg4, Arg5) ->
        {ok, integer(), Val2, Val3, Val4, Val5} | {error, posix()}
    prctl(Drv, Pids, Option, Arg2, Arg3, Arg4, Arg5) ->
        {ok, integer(), Val2, Val3, Val4, Val5} | {error, posix()}

        Types   Option = integer()
                Arg2 = Arg3 = Arg4 = Arg5 = integer() | iodata()

        Linux only.

        prctl(2) : operations on a process

        This function can be used to set BPF syscall filters on processes
        (seccomp mode).

    prctl_define(Drv, atom()) -> integer() | unknown
    prctl_define(Drv, Pids, atom()) -> integer() | unknown

        Convert prctl option names to integers.

    read(Drv, Fd, Count) -> {ok, binary()} | {error, posix()}
    read(Drv, Pids, Fd, Count) -> {ok, binary()} | {error, posix()}

        Types   Count = non_neg_integer()

        read(2) : read bytes from a file descriptor

    readdir(Drv, Path) -> {ok, [binary()]} | {error, posix()}
    readdir(Drv, Pids, Path) -> {ok, [binary()]} | {error, posix()}

        readdir(3) : retrieve list of objects in a directory

    rlimit_define(Drv, atom()) -> integer() | unknown
    rlimit_define(Drv, Pids, atom()) -> integer() | unknown

        Convert an RLIMIT_* flag to an integer().

    rmdir(Drv, Path) -> ok | {error, posix()}
    rmdir(Drv, Pids, Path) -> ok | {error, posix()}

        rmdir(2) : delete a directory

    select(Drv, Readfds, Writefds, Exceptfds, Timeout) -> {ok, Readfds, Writefds, Exceptfds} | {error, posix()}
    select(Drv, Pids, Readfds, Writefds, Exceptfds, Timeout) -> {ok, Readfds, Writefds, Exceptfds} | {error, posix()}

        Types   Readfds = Writefds = Exceptfds = [] | [integer()]
                Timeout = <<>> | #alcove_timeval{}

        select(2) : poll a list of file descriptor for events

        select/5,6 will block until an event occurs on a file descriptor,
        a timeout is reached or interrupted by a signal.

        The Timeout value may be:

            * an empty binary (<<>>) signifying no value (block forever)

            * an alcove_timeval record with these fields:

                sec : number of seconds to wait
                usec : number of microseconds to wait

    setenv(Drv, Name, Value, Overwrite) -> ok | {error, posix()}
    setenv(Drv, Pids, Name, Value, Overwrite) -> ok | {error, posix()}

        Types   Name = Value = iodata()
                Overwrite = 0 | 1

        setenv(3) : set an environment variable

    setgid(Drv, Gid) -> ok | {error, posix()}
    setgid(Drv, Pids, Gid) -> ok | {error, posix()}

        Types   Gid = non_neg_integer()

        setgid(2) : set the GID of the process

    setpgid(Drv, Pid, Pgid) -> ok | {error, posix()}
    setpgid(Drv, Pids,Pid, Pgid) -> ok | {error, posix()}

        Types   Pgid = integer()

        setpgid(2) : set process group

    setsid(Drv) -> {ok, Pid} | {error, posix()}
    setsid(Drv, Pids) -> {ok, Pid} | {error, posix()}

        setsid(2) : create a new session

    sethostname(Drv, Hostname) -> ok | {error, posix()}
    sethostname(Drv, Pids, Hostname) -> ok | {error, posix()}

        Types   Hostname = iodata()

        sethostname(2) : set the system hostname

        This function is probably only useful if running in a uts namespace:

            Flags = alcove:define(Drv, 'CLONE_NEWUTS'),
            {ok, Child} = alcove:clone(Drv, Flags),
            ok = alcove:sethostname(Drv, [Child], "test"),
            Hostname1 = alcove:gethostname(Drv),
            Hostname2 = alcove:gethostname(Drv, [Child]),
            Hostname1 =/= Hostname2.

    setns(Drv, Path) -> ok | {error, posix()}
    setns(Drv, Pids, Path) -> ok | {error, posix()}

        Linux only.

        setns(2) : attach to a namespace

        A process namespace is represented as a path in the /proc
        filesystem. The path is /proc/<pid>/ns/<ns>, where:

            pid = the system PID
            ns = a file representing the namespace

        The available namespaces is dependent on the kernel version. You
        can see which are supported by running:

            ls -al /proc/$$/ns

        On Ubuntu 12.04, the ipc, net and uts namespaces are available.

        For example, to attach to another process' network namespace:

            Flags = alcove:define(Drv, clone_newnet),
            {ok, Child1} = alcove:clone(Drv, Flags),
            {ok, Child2} = alcove:fork(Drv),

            % Move Child2 into the Child1 network namespace
            ok = alcove:setns(Drv, [Child2],
                    ["/proc/", integer_to_list(Child1), "/ns/net"]).

    setopt(Drv, Opt, Val) -> boolean()
    setopt(Drv, Pids, Opt, Val) -> boolean()

        Set port options. See getopt/2,3 for the list of options.

    setrlimit(Drv, Resource, Limit) -> ok | {error, posix()}
    setrlimit(Drv, Pids, Resource, Limit) -> ok | {error, posix()}

        Types   Opt = integer()
                Val = #alcove_rlimit{}

        setrlimit(2) : set a resource limit

    setuid(Drv, UID) -> ok | {error, posix()}
    setuid(Drv, Pids, UID) -> ok | {error, posix()}

        Types   UID = non_neg_integer()

        setuid(2) : change UID

    sigaction(Drv, Signum, Handler) -> ok | {error, posix()}
    sigaction(Drv, Pids, Signum, Handler) -> ok | {error, posix()}

        Types   Signum = integer() | atom()
                Handler = dfl | ign | trap

        sigaction(2) : set process behaviour for signals

            dfl : uses the default behaviour for the signal

            ign : ignores the signal

            trap : catches the signal and sends the controlling Erlang
                   process an event, {signal, atom()}

        Multiple trapped signals may be reported as one event.

    signal_constant(Drv, integer()) -> atom() | unknown
    signal_constant(Drv, Pids, integer()) -> atom() | unknown

        Convert integers to signal names.

    signal_define(Drv, atom()) -> integer() | unknown
    signal_define(Drv, Pids, atom()) -> integer() | unknown

        Convert signal names to integers.

    umount(Drv, Path) -> ok | {error, posix()}
    umount(Drv, Pids, Path) -> ok | {error, posix()}

        umount(2) : unmount a filesystem

        On BSD systems, calls unmount(2).

    unsetenv(Drv, Name) -> ok | {error, posix()}
    unsetenv(Drv, Pids, Name) -> ok | {error, posix()}

        unsetenv(3) : remove an environment variable

    unshare(Drv, Flags) -> ok | {error, posix()}
    unshare(Drv, Pids, Flags) -> ok | {error, posix()}

        Types   Flags = integer()

        Linux only.

        unshare(2) : allows creating a new namespace in the current process

        unshare(2) lets you make a new namespace without calling clone(2):

            Flags = alcove:define(Drv, clone_newnet),
            ok = alcove:unshare(Drv, Flags).

            % The port is now running in a namespace without network access.

    version(Drv) -> binary()
    version(Drv, Pids) -> binary()

        Retrieves the alcove version.

    write(Drv, FD, Buf) -> {ok, Count} | {error, posix()}
    write(Drv, Pids, FD, Buf) -> {ok, Count} | {error, posix()}

        Types   Buf = iodata()
                Count = non_neg_integer()

        Writes a buffer to a file descriptor and returns the number of
        bytes written.

The alcove module functions can be rewritten to use call/2,3,4,5 which
allows setting timeouts.

    call(Drv, Call) -> term()
    call(Drv, Call, Argv) -> term()
    call(Drv, Pids, Call, Argv) -> term()
    call(Drv, Pids, Call, Argv, Timeout) -> term()

        Types   Call = chdir | chmod | chown | chroot | ...
                Argv = [iodata()]
                Timeout = timeout()

        Make a synchronous call into the port driver.

            alcove:call(Drv, Pids, execve,
                    ["/bin/ls", ["/bin/ls", "-al"], ["HOME=/home/foo"]])

        By default, timeout is set to infinity. Similar to
        gen_server:call/3, setting an integer timeout will cause the
        process to crash if the timeout is reached. If the failure is
        caught, the caller must deal with any delayed messages that
        arrive for the Unix process described by the fork path.

        See "Message Format" for a description of the messages.

### Standard I/0

These functions handle stdin, stdout and stderr for the processs after
exec(3) has been called.

    stdin(Drv, Pids, Buf) -> true

        Types   Buf = iodata()

        Send data to stdin of the process.

    stdout(Drv, Pids) -> binary() | false

        Read stdout from the process.

    stderr(Drv, Pids) -> binary() | false

        Read stderr from the process.

    eof(Drv, Pids) -> ok | {error, posix()}

        Close stdin of the process.

Message Format
==============

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
========

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

Tests
=====

* Linux

The tests rely on having a statically linked version of busybox. On
Ubuntu:

    apt-get install busybox-static

A statically linked executable is required because the tests do a
chroot(2) to /bin before exec'ing the binary.
