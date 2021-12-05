% Copyright (c) 2021, Michael Santos <michael.santos@gmail.com>
% Permission to use, copy, modify, and/or distribute this software for any
% purpose with or without fee is hereby granted, provided that the above
% copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%
%%% % @noformat

-module(alcove_proto).

-include("alcove.hrl").

-export_type([call/0]).

-type call() :: alloc | cap_constant | cap_enter | cap_fcntls_get | cap_fcntls_limit | cap_getmode | cap_ioctls_limit | cap_rights_limit | chdir | chmod | chown | chroot | clearenv | clone | clone_constant | close | connect | cpid | environ | errno_id | execve | execvp | exit | fcntl | fcntl_constant | fexecve | file_constant | filter | fork | getcwd | getenv | getgid | getgroups | gethostname | getopt | getpgrp | getpid | getpriority | getresgid | getresuid | getrlimit | getsid | getuid | ioctl | ioctl_constant | iolist_to_bin | jail | jail_attach | jail_remove | kill | link | lseek | mkdir | mkfifo | mount | mount_constant | open | pivot_root | pledge | prctl | prctl_constant | procctl | ptrace | ptrace_constant | read | readdir | rlimit_constant | rmdir | seccomp | seccomp_constant | select | setcpid | setenv | setgid | setgroups | sethostname | setns | setopt | setpgid | setpriority | setproctitle | setresgid | setresuid | setrlimit | setsid | setuid | sigaction | signal_constant | socket | symlink | syscall_constant | umount | umount2 | unlink | unsetenv | unshare | unveil | version | waitpid | write.

-spec call(call()) -> 0..100.
-spec will_return(call()) -> boolean().
-spec calls() -> [call(),...].

% Static functions

-export([call/1, will_return/1]).

% Generated functions

-export([calls/0]).


call(Call) when is_atom(Call) ->
    lookup(Call, calls(), 0).

lookup(Call, [Call|_], N) ->
    N;
lookup(Call, [_|Calls], N) ->
    lookup(Call, Calls, N+1).

will_return(execve) -> false;
will_return(execvp) -> false;
will_return(exit) -> false;
will_return(fexecve) -> false;
will_return(_) -> true.


calls() ->
    [alloc,
     cap_constant,
     cap_enter,
     cap_fcntls_get,
     cap_fcntls_limit,
     cap_getmode,
     cap_ioctls_limit,
     cap_rights_limit,
     chdir,
     chmod,
     chown,
     chroot,
     clearenv,
     clone,
     clone_constant,
     close,
     connect,
     cpid,
     environ,
     errno_id,
     execve,
     execvp,
     exit,
     fcntl,
     fcntl_constant,
     fexecve,
     file_constant,
     filter,
     fork,
     getcwd,
     getenv,
     getgid,
     getgroups,
     gethostname,
     getopt,
     getpgrp,
     getpid,
     getpriority,
     getresgid,
     getresuid,
     getrlimit,
     getsid,
     getuid,
     ioctl,
     ioctl_constant,
     iolist_to_bin,
     jail,
     jail_attach,
     jail_remove,
     kill,
     link,
     lseek,
     mkdir,
     mkfifo,
     mount,
     mount_constant,
     open,
     pivot_root,
     pledge,
     prctl,
     prctl_constant,
     procctl,
     ptrace,
     ptrace_constant,
     read,
     readdir,
     rlimit_constant,
     rmdir,
     seccomp,
     seccomp_constant,
     select,
     setcpid,
     setenv,
     setgid,
     setgroups,
     sethostname,
     setns,
     setopt,
     setpgid,
     setpriority,
     setproctitle,
     setresgid,
     setresuid,
     setrlimit,
     setsid,
     setuid,
     sigaction,
     signal_constant,
     socket,
     symlink,
     syscall_constant,
     umount,
     umount2,
     unlink,
     unsetenv,
     unshare,
     unveil,
     version,
     waitpid,
     write].
