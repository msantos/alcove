/* Copyright (c) 2014, Michael Santos <michael.santos@gmail.com>
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include <sys/syscall.h>

const alcove_define_t alcove_syscall_constants[] = {
#ifdef SYS_OABI_SYSCALL_BASE
    ALCOVE_DEFINE(SYS_OABI_SYSCALL_BASE),
#endif
#ifdef SYS_SYSCALL_BASE
    ALCOVE_DEFINE(SYS_SYSCALL_BASE),
#endif
#ifdef SYS_SYSCALL_BASE
    ALCOVE_DEFINE(SYS_SYSCALL_BASE),
#endif
#ifdef SYS_restart_syscall
    ALCOVE_DEFINE(SYS_restart_syscall),
#endif
#ifdef SYS_exit
    ALCOVE_DEFINE(SYS_exit),
#endif
#ifdef SYS_fork
    ALCOVE_DEFINE(SYS_fork),
#endif
#ifdef SYS_read
    ALCOVE_DEFINE(SYS_read),
#endif
#ifdef SYS_write
    ALCOVE_DEFINE(SYS_write),
#endif
#ifdef SYS_open
    ALCOVE_DEFINE(SYS_open),
#endif
#ifdef SYS_close
    ALCOVE_DEFINE(SYS_close),
#endif
#ifdef SYS_creat
    ALCOVE_DEFINE(SYS_creat),
#endif
#ifdef SYS_link
    ALCOVE_DEFINE(SYS_link),
#endif
#ifdef SYS_unlink
    ALCOVE_DEFINE(SYS_unlink),
#endif
#ifdef SYS_execve
    ALCOVE_DEFINE(SYS_execve),
#endif
#ifdef SYS_chdir
    ALCOVE_DEFINE(SYS_chdir),
#endif
#ifdef SYS_time
    ALCOVE_DEFINE(SYS_time),
#endif
#ifdef SYS_mknod
    ALCOVE_DEFINE(SYS_mknod),
#endif
#ifdef SYS_chmod
    ALCOVE_DEFINE(SYS_chmod),
#endif
#ifdef SYS_lchown
    ALCOVE_DEFINE(SYS_lchown),
#endif
#ifdef SYS_lseek
    ALCOVE_DEFINE(SYS_lseek),
#endif
#ifdef SYS_getpid
    ALCOVE_DEFINE(SYS_getpid),
#endif
#ifdef SYS_mount
    ALCOVE_DEFINE(SYS_mount),
#endif
#ifdef SYS_umount
    ALCOVE_DEFINE(SYS_umount),
#endif
#ifdef SYS_setuid
    ALCOVE_DEFINE(SYS_setuid),
#endif
#ifdef SYS_getuid
    ALCOVE_DEFINE(SYS_getuid),
#endif
#ifdef SYS_stime
    ALCOVE_DEFINE(SYS_stime),
#endif
#ifdef SYS_ptrace
    ALCOVE_DEFINE(SYS_ptrace),
#endif
#ifdef SYS_alarm
    ALCOVE_DEFINE(SYS_alarm),
#endif
#ifdef SYS_pause
    ALCOVE_DEFINE(SYS_pause),
#endif
#ifdef SYS_utime
    ALCOVE_DEFINE(SYS_utime),
#endif
#ifdef SYS_access
    ALCOVE_DEFINE(SYS_access),
#endif
#ifdef SYS_nice
    ALCOVE_DEFINE(SYS_nice),
#endif
#ifdef SYS_sync
    ALCOVE_DEFINE(SYS_sync),
#endif
#ifdef SYS_kill
    ALCOVE_DEFINE(SYS_kill),
#endif
#ifdef SYS_rename
    ALCOVE_DEFINE(SYS_rename),
#endif
#ifdef SYS_mkdir
    ALCOVE_DEFINE(SYS_mkdir),
#endif
#ifdef SYS_rmdir
    ALCOVE_DEFINE(SYS_rmdir),
#endif
#ifdef SYS_dup
    ALCOVE_DEFINE(SYS_dup),
#endif
#ifdef SYS_pipe
    ALCOVE_DEFINE(SYS_pipe),
#endif
#ifdef SYS_times
    ALCOVE_DEFINE(SYS_times),
#endif
#ifdef SYS_brk
    ALCOVE_DEFINE(SYS_brk),
#endif
#ifdef SYS_setgid
    ALCOVE_DEFINE(SYS_setgid),
#endif
#ifdef SYS_getgid
    ALCOVE_DEFINE(SYS_getgid),
#endif
#ifdef SYS_geteuid
    ALCOVE_DEFINE(SYS_geteuid),
#endif
#ifdef SYS_getegid
    ALCOVE_DEFINE(SYS_getegid),
#endif
#ifdef SYS_acct
    ALCOVE_DEFINE(SYS_acct),
#endif
#ifdef SYS_umount2
    ALCOVE_DEFINE(SYS_umount2),
#endif
#ifdef SYS_ioctl
    ALCOVE_DEFINE(SYS_ioctl),
#endif
#ifdef SYS_fcntl
    ALCOVE_DEFINE(SYS_fcntl),
#endif
#ifdef SYS_setpgid
    ALCOVE_DEFINE(SYS_setpgid),
#endif
#ifdef SYS_umask
    ALCOVE_DEFINE(SYS_umask),
#endif
#ifdef SYS_chroot
    ALCOVE_DEFINE(SYS_chroot),
#endif
#ifdef SYS_ustat
    ALCOVE_DEFINE(SYS_ustat),
#endif
#ifdef SYS_dup2
    ALCOVE_DEFINE(SYS_dup2),
#endif
#ifdef SYS_getppid
    ALCOVE_DEFINE(SYS_getppid),
#endif
#ifdef SYS_getpgrp
    ALCOVE_DEFINE(SYS_getpgrp),
#endif
#ifdef SYS_setsid
    ALCOVE_DEFINE(SYS_setsid),
#endif
#ifdef SYS_sigaction
    ALCOVE_DEFINE(SYS_sigaction),
#endif
#ifdef SYS_setreuid
    ALCOVE_DEFINE(SYS_setreuid),
#endif
#ifdef SYS_setregid
    ALCOVE_DEFINE(SYS_setregid),
#endif
#ifdef SYS_sigsuspend
    ALCOVE_DEFINE(SYS_sigsuspend),
#endif
#ifdef SYS_sigpending
    ALCOVE_DEFINE(SYS_sigpending),
#endif
#ifdef SYS_sethostname
    ALCOVE_DEFINE(SYS_sethostname),
#endif
#ifdef SYS_setrlimit
    ALCOVE_DEFINE(SYS_setrlimit),
#endif
#ifdef SYS_getrlimit
    ALCOVE_DEFINE(SYS_getrlimit),
#endif
#ifdef SYS_getrusage
    ALCOVE_DEFINE(SYS_getrusage),
#endif
#ifdef SYS_gettimeofday
    ALCOVE_DEFINE(SYS_gettimeofday),
#endif
#ifdef SYS_settimeofday
    ALCOVE_DEFINE(SYS_settimeofday),
#endif
#ifdef SYS_getgroups
    ALCOVE_DEFINE(SYS_getgroups),
#endif
#ifdef SYS_setgroups
    ALCOVE_DEFINE(SYS_setgroups),
#endif
#ifdef SYS_select
    ALCOVE_DEFINE(SYS_select),
#endif
#ifdef SYS_symlink
    ALCOVE_DEFINE(SYS_symlink),
#endif
#ifdef SYS_readlink
    ALCOVE_DEFINE(SYS_readlink),
#endif
#ifdef SYS_uselib
    ALCOVE_DEFINE(SYS_uselib),
#endif
#ifdef SYS_swapon
    ALCOVE_DEFINE(SYS_swapon),
#endif
#ifdef SYS_reboot
    ALCOVE_DEFINE(SYS_reboot),
#endif
#ifdef SYS_readdir
    ALCOVE_DEFINE(SYS_readdir),
#endif
#ifdef SYS_mmap
    ALCOVE_DEFINE(SYS_mmap),
#endif
#ifdef SYS_munmap
    ALCOVE_DEFINE(SYS_munmap),
#endif
#ifdef SYS_truncate
    ALCOVE_DEFINE(SYS_truncate),
#endif
#ifdef SYS_ftruncate
    ALCOVE_DEFINE(SYS_ftruncate),
#endif
#ifdef SYS_fchmod
    ALCOVE_DEFINE(SYS_fchmod),
#endif
#ifdef SYS_fchown
    ALCOVE_DEFINE(SYS_fchown),
#endif
#ifdef SYS_getpriority
    ALCOVE_DEFINE(SYS_getpriority),
#endif
#ifdef SYS_setpriority
    ALCOVE_DEFINE(SYS_setpriority),
#endif
#ifdef SYS_statfs
    ALCOVE_DEFINE(SYS_statfs),
#endif
#ifdef SYS_fstatfs
    ALCOVE_DEFINE(SYS_fstatfs),
#endif
#ifdef SYS_socketcall
    ALCOVE_DEFINE(SYS_socketcall),
#endif
#ifdef SYS_syslog
    ALCOVE_DEFINE(SYS_syslog),
#endif
#ifdef SYS_setitimer
    ALCOVE_DEFINE(SYS_setitimer),
#endif
#ifdef SYS_getitimer
    ALCOVE_DEFINE(SYS_getitimer),
#endif
#ifdef SYS_stat
    ALCOVE_DEFINE(SYS_stat),
#endif
#ifdef SYS_lstat
    ALCOVE_DEFINE(SYS_lstat),
#endif
#ifdef SYS_fstat
    ALCOVE_DEFINE(SYS_fstat),
#endif
#ifdef SYS_vhangup
    ALCOVE_DEFINE(SYS_vhangup),
#endif
#ifdef SYS_syscall
    ALCOVE_DEFINE(SYS_syscall),
#endif
#ifdef SYS_wait4
    ALCOVE_DEFINE(SYS_wait4),
#endif
#ifdef SYS_swapoff
    ALCOVE_DEFINE(SYS_swapoff),
#endif
#ifdef SYS_sysinfo
    ALCOVE_DEFINE(SYS_sysinfo),
#endif
#ifdef SYS_ipc
    ALCOVE_DEFINE(SYS_ipc),
#endif
#ifdef SYS_fsync
    ALCOVE_DEFINE(SYS_fsync),
#endif
#ifdef SYS_sigreturn
    ALCOVE_DEFINE(SYS_sigreturn),
#endif
#ifdef SYS_clone
    ALCOVE_DEFINE(SYS_clone),
#endif
#ifdef SYS_setdomainname
    ALCOVE_DEFINE(SYS_setdomainname),
#endif
#ifdef SYS_uname
    ALCOVE_DEFINE(SYS_uname),
#endif
#ifdef SYS_adjtimex
    ALCOVE_DEFINE(SYS_adjtimex),
#endif
#ifdef SYS_mprotect
    ALCOVE_DEFINE(SYS_mprotect),
#endif
#ifdef SYS_sigprocmask
    ALCOVE_DEFINE(SYS_sigprocmask),
#endif
#ifdef SYS_init_module
    ALCOVE_DEFINE(SYS_init_module),
#endif
#ifdef SYS_delete_module
    ALCOVE_DEFINE(SYS_delete_module),
#endif
#ifdef SYS_quotactl
    ALCOVE_DEFINE(SYS_quotactl),
#endif
#ifdef SYS_getpgid
    ALCOVE_DEFINE(SYS_getpgid),
#endif
#ifdef SYS_fchdir
    ALCOVE_DEFINE(SYS_fchdir),
#endif
#ifdef SYS_bdflush
    ALCOVE_DEFINE(SYS_bdflush),
#endif
#ifdef SYS_sysfs
    ALCOVE_DEFINE(SYS_sysfs),
#endif
#ifdef SYS_personality
    ALCOVE_DEFINE(SYS_personality),
#endif
#ifdef SYS_setfsuid
    ALCOVE_DEFINE(SYS_setfsuid),
#endif
#ifdef SYS_setfsgid
    ALCOVE_DEFINE(SYS_setfsgid),
#endif
#ifdef SYS__llseek
    ALCOVE_DEFINE(SYS__llseek),
#endif
#ifdef SYS_getdents
    ALCOVE_DEFINE(SYS_getdents),
#endif
#ifdef SYS__newselect
    ALCOVE_DEFINE(SYS__newselect),
#endif
#ifdef SYS_flock
    ALCOVE_DEFINE(SYS_flock),
#endif
#ifdef SYS_msync
    ALCOVE_DEFINE(SYS_msync),
#endif
#ifdef SYS_readv
    ALCOVE_DEFINE(SYS_readv),
#endif
#ifdef SYS_writev
    ALCOVE_DEFINE(SYS_writev),
#endif
#ifdef SYS_getsid
    ALCOVE_DEFINE(SYS_getsid),
#endif
#ifdef SYS_fdatasync
    ALCOVE_DEFINE(SYS_fdatasync),
#endif
#ifdef SYS__sysctl
    ALCOVE_DEFINE(SYS__sysctl),
#endif
#ifdef SYS_mlock
    ALCOVE_DEFINE(SYS_mlock),
#endif
#ifdef SYS_munlock
    ALCOVE_DEFINE(SYS_munlock),
#endif
#ifdef SYS_mlockall
    ALCOVE_DEFINE(SYS_mlockall),
#endif
#ifdef SYS_munlockall
    ALCOVE_DEFINE(SYS_munlockall),
#endif
#ifdef SYS_sched_setparam
    ALCOVE_DEFINE(SYS_sched_setparam),
#endif
#ifdef SYS_sched_getparam
    ALCOVE_DEFINE(SYS_sched_getparam),
#endif
#ifdef SYS_sched_setscheduler
    ALCOVE_DEFINE(SYS_sched_setscheduler),
#endif
#ifdef SYS_sched_getscheduler
    ALCOVE_DEFINE(SYS_sched_getscheduler),
#endif
#ifdef SYS_sched_yield
    ALCOVE_DEFINE(SYS_sched_yield),
#endif
#ifdef SYS_sched_get_priority_max
    ALCOVE_DEFINE(SYS_sched_get_priority_max),
#endif
#ifdef SYS_sched_get_priority_min
    ALCOVE_DEFINE(SYS_sched_get_priority_min),
#endif
#ifdef SYS_sched_rr_get_interval
    ALCOVE_DEFINE(SYS_sched_rr_get_interval),
#endif
#ifdef SYS_nanosleep
    ALCOVE_DEFINE(SYS_nanosleep),
#endif
#ifdef SYS_mremap
    ALCOVE_DEFINE(SYS_mremap),
#endif
#ifdef SYS_setresuid
    ALCOVE_DEFINE(SYS_setresuid),
#endif
#ifdef SYS_getresuid
    ALCOVE_DEFINE(SYS_getresuid),
#endif
#ifdef SYS_poll
    ALCOVE_DEFINE(SYS_poll),
#endif
#ifdef SYS_nfsservctl
    ALCOVE_DEFINE(SYS_nfsservctl),
#endif
#ifdef SYS_setresgid
    ALCOVE_DEFINE(SYS_setresgid),
#endif
#ifdef SYS_getresgid
    ALCOVE_DEFINE(SYS_getresgid),
#endif
#ifdef SYS_prctl
    ALCOVE_DEFINE(SYS_prctl),
#endif
#ifdef SYS_rt_sigreturn
    ALCOVE_DEFINE(SYS_rt_sigreturn),
#endif
#ifdef SYS_rt_sigaction
    ALCOVE_DEFINE(SYS_rt_sigaction),
#endif
#ifdef SYS_rt_sigprocmask
    ALCOVE_DEFINE(SYS_rt_sigprocmask),
#endif
#ifdef SYS_rt_sigpending
    ALCOVE_DEFINE(SYS_rt_sigpending),
#endif
#ifdef SYS_rt_sigtimedwait
    ALCOVE_DEFINE(SYS_rt_sigtimedwait),
#endif
#ifdef SYS_rt_sigqueueinfo
    ALCOVE_DEFINE(SYS_rt_sigqueueinfo),
#endif
#ifdef SYS_rt_sigsuspend
    ALCOVE_DEFINE(SYS_rt_sigsuspend),
#endif
#ifdef SYS_pread64
    ALCOVE_DEFINE(SYS_pread64),
#endif
#ifdef SYS_pwrite64
    ALCOVE_DEFINE(SYS_pwrite64),
#endif
#ifdef SYS_chown
    ALCOVE_DEFINE(SYS_chown),
#endif
#ifdef SYS_getcwd
    ALCOVE_DEFINE(SYS_getcwd),
#endif
#ifdef SYS_capget
    ALCOVE_DEFINE(SYS_capget),
#endif
#ifdef SYS_capset
    ALCOVE_DEFINE(SYS_capset),
#endif
#ifdef SYS_sigaltstack
    ALCOVE_DEFINE(SYS_sigaltstack),
#endif
#ifdef SYS_sendfile
    ALCOVE_DEFINE(SYS_sendfile),
#endif
#ifdef SYS_vfork
    ALCOVE_DEFINE(SYS_vfork),
#endif
#ifdef SYS_ugetrlimit
    ALCOVE_DEFINE(SYS_ugetrlimit),
#endif
#ifdef SYS_mmap2
    ALCOVE_DEFINE(SYS_mmap2),
#endif
#ifdef SYS_truncate64
    ALCOVE_DEFINE(SYS_truncate64),
#endif
#ifdef SYS_ftruncate64
    ALCOVE_DEFINE(SYS_ftruncate64),
#endif
#ifdef SYS_stat64
    ALCOVE_DEFINE(SYS_stat64),
#endif
#ifdef SYS_lstat64
    ALCOVE_DEFINE(SYS_lstat64),
#endif
#ifdef SYS_fstat64
    ALCOVE_DEFINE(SYS_fstat64),
#endif
#ifdef SYS_lchown32
    ALCOVE_DEFINE(SYS_lchown32),
#endif
#ifdef SYS_getuid32
    ALCOVE_DEFINE(SYS_getuid32),
#endif
#ifdef SYS_getgid32
    ALCOVE_DEFINE(SYS_getgid32),
#endif
#ifdef SYS_geteuid32
    ALCOVE_DEFINE(SYS_geteuid32),
#endif
#ifdef SYS_getegid32
    ALCOVE_DEFINE(SYS_getegid32),
#endif
#ifdef SYS_setreuid32
    ALCOVE_DEFINE(SYS_setreuid32),
#endif
#ifdef SYS_setregid32
    ALCOVE_DEFINE(SYS_setregid32),
#endif
#ifdef SYS_getgroups32
    ALCOVE_DEFINE(SYS_getgroups32),
#endif
#ifdef SYS_setgroups32
    ALCOVE_DEFINE(SYS_setgroups32),
#endif
#ifdef SYS_fchown32
    ALCOVE_DEFINE(SYS_fchown32),
#endif
#ifdef SYS_setresuid32
    ALCOVE_DEFINE(SYS_setresuid32),
#endif
#ifdef SYS_getresuid32
    ALCOVE_DEFINE(SYS_getresuid32),
#endif
#ifdef SYS_setresgid32
    ALCOVE_DEFINE(SYS_setresgid32),
#endif
#ifdef SYS_getresgid32
    ALCOVE_DEFINE(SYS_getresgid32),
#endif
#ifdef SYS_chown32
    ALCOVE_DEFINE(SYS_chown32),
#endif
#ifdef SYS_setuid32
    ALCOVE_DEFINE(SYS_setuid32),
#endif
#ifdef SYS_setgid32
    ALCOVE_DEFINE(SYS_setgid32),
#endif
#ifdef SYS_setfsuid32
    ALCOVE_DEFINE(SYS_setfsuid32),
#endif
#ifdef SYS_setfsgid32
    ALCOVE_DEFINE(SYS_setfsgid32),
#endif
#ifdef SYS_getdents64
    ALCOVE_DEFINE(SYS_getdents64),
#endif
#ifdef SYS_pivot_root
    ALCOVE_DEFINE(SYS_pivot_root),
#endif
#ifdef SYS_mincore
    ALCOVE_DEFINE(SYS_mincore),
#endif
#ifdef SYS_madvise
    ALCOVE_DEFINE(SYS_madvise),
#endif
#ifdef SYS_fcntl64
    ALCOVE_DEFINE(SYS_fcntl64),
#endif
#ifdef SYS_gettid
    ALCOVE_DEFINE(SYS_gettid),
#endif
#ifdef SYS_readahead
    ALCOVE_DEFINE(SYS_readahead),
#endif
#ifdef SYS_setxattr
    ALCOVE_DEFINE(SYS_setxattr),
#endif
#ifdef SYS_lsetxattr
    ALCOVE_DEFINE(SYS_lsetxattr),
#endif
#ifdef SYS_fsetxattr
    ALCOVE_DEFINE(SYS_fsetxattr),
#endif
#ifdef SYS_getxattr
    ALCOVE_DEFINE(SYS_getxattr),
#endif
#ifdef SYS_lgetxattr
    ALCOVE_DEFINE(SYS_lgetxattr),
#endif
#ifdef SYS_fgetxattr
    ALCOVE_DEFINE(SYS_fgetxattr),
#endif
#ifdef SYS_listxattr
    ALCOVE_DEFINE(SYS_listxattr),
#endif
#ifdef SYS_llistxattr
    ALCOVE_DEFINE(SYS_llistxattr),
#endif
#ifdef SYS_flistxattr
    ALCOVE_DEFINE(SYS_flistxattr),
#endif
#ifdef SYS_removexattr
    ALCOVE_DEFINE(SYS_removexattr),
#endif
#ifdef SYS_lremovexattr
    ALCOVE_DEFINE(SYS_lremovexattr),
#endif
#ifdef SYS_fremovexattr
    ALCOVE_DEFINE(SYS_fremovexattr),
#endif
#ifdef SYS_tkill
    ALCOVE_DEFINE(SYS_tkill),
#endif
#ifdef SYS_sendfile64
    ALCOVE_DEFINE(SYS_sendfile64),
#endif
#ifdef SYS_futex
    ALCOVE_DEFINE(SYS_futex),
#endif
#ifdef SYS_sched_setaffinity
    ALCOVE_DEFINE(SYS_sched_setaffinity),
#endif
#ifdef SYS_sched_getaffinity
    ALCOVE_DEFINE(SYS_sched_getaffinity),
#endif
#ifdef SYS_io_setup
    ALCOVE_DEFINE(SYS_io_setup),
#endif
#ifdef SYS_io_destroy
    ALCOVE_DEFINE(SYS_io_destroy),
#endif
#ifdef SYS_io_getevents
    ALCOVE_DEFINE(SYS_io_getevents),
#endif
#ifdef SYS_io_submit
    ALCOVE_DEFINE(SYS_io_submit),
#endif
#ifdef SYS_io_cancel
    ALCOVE_DEFINE(SYS_io_cancel),
#endif
#ifdef SYS_exit_group
    ALCOVE_DEFINE(SYS_exit_group),
#endif
#ifdef SYS_lookup_dcookie
    ALCOVE_DEFINE(SYS_lookup_dcookie),
#endif
#ifdef SYS_epoll_create
    ALCOVE_DEFINE(SYS_epoll_create),
#endif
#ifdef SYS_epoll_ctl
    ALCOVE_DEFINE(SYS_epoll_ctl),
#endif
#ifdef SYS_epoll_wait
    ALCOVE_DEFINE(SYS_epoll_wait),
#endif
#ifdef SYS_remap_file_pages
    ALCOVE_DEFINE(SYS_remap_file_pages),
#endif
#ifdef SYS_set_tid_address
    ALCOVE_DEFINE(SYS_set_tid_address),
#endif
#ifdef SYS_timer_create
    ALCOVE_DEFINE(SYS_timer_create),
#endif
#ifdef SYS_timer_settime
    ALCOVE_DEFINE(SYS_timer_settime),
#endif
#ifdef SYS_timer_gettime
    ALCOVE_DEFINE(SYS_timer_gettime),
#endif
#ifdef SYS_timer_getoverrun
    ALCOVE_DEFINE(SYS_timer_getoverrun),
#endif
#ifdef SYS_timer_delete
    ALCOVE_DEFINE(SYS_timer_delete),
#endif
#ifdef SYS_clock_settime
    ALCOVE_DEFINE(SYS_clock_settime),
#endif
#ifdef SYS_clock_gettime
    ALCOVE_DEFINE(SYS_clock_gettime),
#endif
#ifdef SYS_clock_getres
    ALCOVE_DEFINE(SYS_clock_getres),
#endif
#ifdef SYS_clock_nanosleep
    ALCOVE_DEFINE(SYS_clock_nanosleep),
#endif
#ifdef SYS_statfs64
    ALCOVE_DEFINE(SYS_statfs64),
#endif
#ifdef SYS_fstatfs64
    ALCOVE_DEFINE(SYS_fstatfs64),
#endif
#ifdef SYS_tgkill
    ALCOVE_DEFINE(SYS_tgkill),
#endif
#ifdef SYS_utimes
    ALCOVE_DEFINE(SYS_utimes),
#endif
#ifdef SYS_arm_fadvise64_64
    ALCOVE_DEFINE(SYS_arm_fadvise64_64),
#endif
#ifdef SYS_pciconfig_iobase
    ALCOVE_DEFINE(SYS_pciconfig_iobase),
#endif
#ifdef SYS_pciconfig_read
    ALCOVE_DEFINE(SYS_pciconfig_read),
#endif
#ifdef SYS_pciconfig_write
    ALCOVE_DEFINE(SYS_pciconfig_write),
#endif
#ifdef SYS_mq_open
    ALCOVE_DEFINE(SYS_mq_open),
#endif
#ifdef SYS_mq_unlink
    ALCOVE_DEFINE(SYS_mq_unlink),
#endif
#ifdef SYS_mq_timedsend
    ALCOVE_DEFINE(SYS_mq_timedsend),
#endif
#ifdef SYS_mq_timedreceive
    ALCOVE_DEFINE(SYS_mq_timedreceive),
#endif
#ifdef SYS_mq_notify
    ALCOVE_DEFINE(SYS_mq_notify),
#endif
#ifdef SYS_mq_getsetattr
    ALCOVE_DEFINE(SYS_mq_getsetattr),
#endif
#ifdef SYS_waitid
    ALCOVE_DEFINE(SYS_waitid),
#endif
#ifdef SYS_socket
    ALCOVE_DEFINE(SYS_socket),
#endif
#ifdef SYS_bind
    ALCOVE_DEFINE(SYS_bind),
#endif
#ifdef SYS_connect
    ALCOVE_DEFINE(SYS_connect),
#endif
#ifdef SYS_listen
    ALCOVE_DEFINE(SYS_listen),
#endif
#ifdef SYS_accept
    ALCOVE_DEFINE(SYS_accept),
#endif
#ifdef SYS_getsockname
    ALCOVE_DEFINE(SYS_getsockname),
#endif
#ifdef SYS_getpeername
    ALCOVE_DEFINE(SYS_getpeername),
#endif
#ifdef SYS_socketpair
    ALCOVE_DEFINE(SYS_socketpair),
#endif
#ifdef SYS_send
    ALCOVE_DEFINE(SYS_send),
#endif
#ifdef SYS_sendto
    ALCOVE_DEFINE(SYS_sendto),
#endif
#ifdef SYS_recv
    ALCOVE_DEFINE(SYS_recv),
#endif
#ifdef SYS_recvfrom
    ALCOVE_DEFINE(SYS_recvfrom),
#endif
#ifdef SYS_shutdown
    ALCOVE_DEFINE(SYS_shutdown),
#endif
#ifdef SYS_setsockopt
    ALCOVE_DEFINE(SYS_setsockopt),
#endif
#ifdef SYS_getsockopt
    ALCOVE_DEFINE(SYS_getsockopt),
#endif
#ifdef SYS_sendmsg
    ALCOVE_DEFINE(SYS_sendmsg),
#endif
#ifdef SYS_recvmsg
    ALCOVE_DEFINE(SYS_recvmsg),
#endif
#ifdef SYS_semop
    ALCOVE_DEFINE(SYS_semop),
#endif
#ifdef SYS_semget
    ALCOVE_DEFINE(SYS_semget),
#endif
#ifdef SYS_semctl
    ALCOVE_DEFINE(SYS_semctl),
#endif
#ifdef SYS_msgsnd
    ALCOVE_DEFINE(SYS_msgsnd),
#endif
#ifdef SYS_msgrcv
    ALCOVE_DEFINE(SYS_msgrcv),
#endif
#ifdef SYS_msgget
    ALCOVE_DEFINE(SYS_msgget),
#endif
#ifdef SYS_msgctl
    ALCOVE_DEFINE(SYS_msgctl),
#endif
#ifdef SYS_shmat
    ALCOVE_DEFINE(SYS_shmat),
#endif
#ifdef SYS_shmdt
    ALCOVE_DEFINE(SYS_shmdt),
#endif
#ifdef SYS_shmget
    ALCOVE_DEFINE(SYS_shmget),
#endif
#ifdef SYS_shmctl
    ALCOVE_DEFINE(SYS_shmctl),
#endif
#ifdef SYS_add_key
    ALCOVE_DEFINE(SYS_add_key),
#endif
#ifdef SYS_request_key
    ALCOVE_DEFINE(SYS_request_key),
#endif
#ifdef SYS_keyctl
    ALCOVE_DEFINE(SYS_keyctl),
#endif
#ifdef SYS_semtimedop
    ALCOVE_DEFINE(SYS_semtimedop),
#endif
#ifdef SYS_vserver
    ALCOVE_DEFINE(SYS_vserver),
#endif
#ifdef SYS_ioprio_set
    ALCOVE_DEFINE(SYS_ioprio_set),
#endif
#ifdef SYS_ioprio_get
    ALCOVE_DEFINE(SYS_ioprio_get),
#endif
#ifdef SYS_inotify_init
    ALCOVE_DEFINE(SYS_inotify_init),
#endif
#ifdef SYS_inotify_add_watch
    ALCOVE_DEFINE(SYS_inotify_add_watch),
#endif
#ifdef SYS_inotify_rm_watch
    ALCOVE_DEFINE(SYS_inotify_rm_watch),
#endif
#ifdef SYS_mbind
    ALCOVE_DEFINE(SYS_mbind),
#endif
#ifdef SYS_get_mempolicy
    ALCOVE_DEFINE(SYS_get_mempolicy),
#endif
#ifdef SYS_set_mempolicy
    ALCOVE_DEFINE(SYS_set_mempolicy),
#endif
#ifdef SYS_openat
    ALCOVE_DEFINE(SYS_openat),
#endif
#ifdef SYS_mkdirat
    ALCOVE_DEFINE(SYS_mkdirat),
#endif
#ifdef SYS_mknodat
    ALCOVE_DEFINE(SYS_mknodat),
#endif
#ifdef SYS_fchownat
    ALCOVE_DEFINE(SYS_fchownat),
#endif
#ifdef SYS_futimesat
    ALCOVE_DEFINE(SYS_futimesat),
#endif
#ifdef SYS_fstatat64
    ALCOVE_DEFINE(SYS_fstatat64),
#endif
#ifdef SYS_unlinkat
    ALCOVE_DEFINE(SYS_unlinkat),
#endif
#ifdef SYS_renameat
    ALCOVE_DEFINE(SYS_renameat),
#endif
#ifdef SYS_linkat
    ALCOVE_DEFINE(SYS_linkat),
#endif
#ifdef SYS_symlinkat
    ALCOVE_DEFINE(SYS_symlinkat),
#endif
#ifdef SYS_readlinkat
    ALCOVE_DEFINE(SYS_readlinkat),
#endif
#ifdef SYS_fchmodat
    ALCOVE_DEFINE(SYS_fchmodat),
#endif
#ifdef SYS_faccessat
    ALCOVE_DEFINE(SYS_faccessat),
#endif
#ifdef SYS_pselect6
    ALCOVE_DEFINE(SYS_pselect6),
#endif
#ifdef SYS_ppoll
    ALCOVE_DEFINE(SYS_ppoll),
#endif
#ifdef SYS_unshare
    ALCOVE_DEFINE(SYS_unshare),
#endif
#ifdef SYS_set_robust_list
    ALCOVE_DEFINE(SYS_set_robust_list),
#endif
#ifdef SYS_get_robust_list
    ALCOVE_DEFINE(SYS_get_robust_list),
#endif
#ifdef SYS_splice
    ALCOVE_DEFINE(SYS_splice),
#endif
#ifdef SYS_arm_sync_file_range
    ALCOVE_DEFINE(SYS_arm_sync_file_range),
#endif
#ifdef SYS_sync_file_range2
    ALCOVE_DEFINE(SYS_sync_file_range2),
#endif
#ifdef SYS_tee
    ALCOVE_DEFINE(SYS_tee),
#endif
#ifdef SYS_vmsplice
    ALCOVE_DEFINE(SYS_vmsplice),
#endif
#ifdef SYS_move_pages
    ALCOVE_DEFINE(SYS_move_pages),
#endif
#ifdef SYS_getcpu
    ALCOVE_DEFINE(SYS_getcpu),
#endif
#ifdef SYS_epoll_pwait
    ALCOVE_DEFINE(SYS_epoll_pwait),
#endif
#ifdef SYS_kexec_load
    ALCOVE_DEFINE(SYS_kexec_load),
#endif
#ifdef SYS_utimensat
    ALCOVE_DEFINE(SYS_utimensat),
#endif
#ifdef SYS_signalfd
    ALCOVE_DEFINE(SYS_signalfd),
#endif
#ifdef SYS_timerfd_create
    ALCOVE_DEFINE(SYS_timerfd_create),
#endif
#ifdef SYS_eventfd
    ALCOVE_DEFINE(SYS_eventfd),
#endif
#ifdef SYS_fallocate
    ALCOVE_DEFINE(SYS_fallocate),
#endif
#ifdef SYS_timerfd_settime
    ALCOVE_DEFINE(SYS_timerfd_settime),
#endif
#ifdef SYS_timerfd_gettime
    ALCOVE_DEFINE(SYS_timerfd_gettime),
#endif
#ifdef SYS_signalfd4
    ALCOVE_DEFINE(SYS_signalfd4),
#endif
#ifdef SYS_eventfd2
    ALCOVE_DEFINE(SYS_eventfd2),
#endif
#ifdef SYS_epoll_create1
    ALCOVE_DEFINE(SYS_epoll_create1),
#endif
#ifdef SYS_dup3
    ALCOVE_DEFINE(SYS_dup3),
#endif
#ifdef SYS_pipe2
    ALCOVE_DEFINE(SYS_pipe2),
#endif
#ifdef SYS_inotify_init1
    ALCOVE_DEFINE(SYS_inotify_init1),
#endif
#ifdef SYS_preadv
    ALCOVE_DEFINE(SYS_preadv),
#endif
#ifdef SYS_pwritev
    ALCOVE_DEFINE(SYS_pwritev),
#endif
#ifdef SYS_rt_tgsigqueueinfo
    ALCOVE_DEFINE(SYS_rt_tgsigqueueinfo),
#endif
#ifdef SYS_perf_event_open
    ALCOVE_DEFINE(SYS_perf_event_open),
#endif
#ifdef SYS_recvmmsg
    ALCOVE_DEFINE(SYS_recvmmsg),
#endif
#ifdef SYS_accept4
    ALCOVE_DEFINE(SYS_accept4),
#endif
#ifdef SYS_fanotify_init
    ALCOVE_DEFINE(SYS_fanotify_init),
#endif
#ifdef SYS_fanotify_mark
    ALCOVE_DEFINE(SYS_fanotify_mark),
#endif
#ifdef SYS_prlimit64
    ALCOVE_DEFINE(SYS_prlimit64),
#endif
#ifdef SYS_name_to_handle_at
    ALCOVE_DEFINE(SYS_name_to_handle_at),
#endif
#ifdef SYS_open_by_handle_at
    ALCOVE_DEFINE(SYS_open_by_handle_at),
#endif
#ifdef SYS_clock_adjtime
    ALCOVE_DEFINE(SYS_clock_adjtime),
#endif
#ifdef SYS_syncfs
    ALCOVE_DEFINE(SYS_syncfs),
#endif
#ifdef SYS_sendmmsg
    ALCOVE_DEFINE(SYS_sendmmsg),
#endif
#ifdef SYS_setns
    ALCOVE_DEFINE(SYS_setns),
#endif
#ifdef SYS_process_vm_readv
    ALCOVE_DEFINE(SYS_process_vm_readv),
#endif
#ifdef SYS_process_vm_writev
    ALCOVE_DEFINE(SYS_process_vm_writev),
#endif

#ifdef AUDIT_ARCH_ALPHA
    ALCOVE_DEFINE(AUDIT_ARCH_ALPHA),
#endif
#ifdef AUDIT_ARCH_ARM
    ALCOVE_DEFINE(AUDIT_ARCH_ARM),
#endif
#ifdef AUDIT_ARCH_ARMEB
    ALCOVE_DEFINE(AUDIT_ARCH_ARMEB),
#endif
#ifdef AUDIT_ARCH_CRIS
    ALCOVE_DEFINE(AUDIT_ARCH_CRIS),
#endif
#ifdef AUDIT_ARCH_FRV
    ALCOVE_DEFINE(AUDIT_ARCH_FRV),
#endif
#ifdef AUDIT_ARCH_H8300
    ALCOVE_DEFINE(AUDIT_ARCH_H8300),
#endif
#ifdef AUDIT_ARCH_I386
    ALCOVE_DEFINE(AUDIT_ARCH_I386),
#endif
#ifdef AUDIT_ARCH_IA64
    ALCOVE_DEFINE(AUDIT_ARCH_IA64),
#endif
#ifdef AUDIT_ARCH_M32R
    ALCOVE_DEFINE(AUDIT_ARCH_M32R),
#endif
#ifdef AUDIT_ARCH_M68K
    ALCOVE_DEFINE(AUDIT_ARCH_M68K),
#endif
#ifdef AUDIT_ARCH_MIPS
    ALCOVE_DEFINE(AUDIT_ARCH_MIPS),
#endif
#ifdef AUDIT_ARCH_MIPSEL
    ALCOVE_DEFINE(AUDIT_ARCH_MIPSEL),
#endif
#ifdef AUDIT_ARCH_MIPS64
    ALCOVE_DEFINE(AUDIT_ARCH_MIPS64),
#endif
#ifdef AUDIT_ARCH_MIPSEL64
    ALCOVE_DEFINE(AUDIT_ARCH_MIPSEL64),
#endif
#ifdef AUDIT_ARCH_PARISC
    ALCOVE_DEFINE(AUDIT_ARCH_PARISC),
#endif
#ifdef AUDIT_ARCH_PARISC64
    ALCOVE_DEFINE(AUDIT_ARCH_PARISC64),
#endif
#ifdef AUDIT_ARCH_PPC
    ALCOVE_DEFINE(AUDIT_ARCH_PPC),
#endif
#ifdef AUDIT_ARCH_PPC64
    ALCOVE_DEFINE(AUDIT_ARCH_PPC64),
#endif
#ifdef AUDIT_ARCH_S390
    ALCOVE_DEFINE(AUDIT_ARCH_S390),
#endif
#ifdef AUDIT_ARCH_S390X
    ALCOVE_DEFINE(AUDIT_ARCH_S390X),
#endif
#ifdef AUDIT_ARCH_SH
    ALCOVE_DEFINE(AUDIT_ARCH_SH),
#endif
#ifdef AUDIT_ARCH_SHEL
    ALCOVE_DEFINE(AUDIT_ARCH_SHEL),
#endif
#ifdef AUDIT_ARCH_SH64
    ALCOVE_DEFINE(AUDIT_ARCH_SH64),
#endif
#ifdef AUDIT_ARCH_SHEL64
    ALCOVE_DEFINE(AUDIT_ARCH_SHEL64),
#endif
#ifdef AUDIT_ARCH_SPARC
    ALCOVE_DEFINE(AUDIT_ARCH_SPARC),
#endif
#ifdef AUDIT_ARCH_SPARC64
    ALCOVE_DEFINE(AUDIT_ARCH_SPARC64),
#endif
#ifdef AUDIT_ARCH_X86_64
    ALCOVE_DEFINE(AUDIT_ARCH_X86_64),
#endif

    {NULL, 0}
};
