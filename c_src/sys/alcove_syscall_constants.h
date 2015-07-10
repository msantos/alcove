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
const alcove_define_t alcove_syscall_constants[] = {
#ifdef __NR_OABI_SYSCALL_BASE
    ALCOVE_DEFINE(__NR_OABI_SYSCALL_BASE),
#endif
#ifdef __NR_SYSCALL_BASE
    ALCOVE_DEFINE(__NR_SYSCALL_BASE),
#endif
#ifdef __NR_SYSCALL_BASE
    ALCOVE_DEFINE(__NR_SYSCALL_BASE),
#endif
#ifdef __NR_restart_syscall
    ALCOVE_DEFINE(__NR_restart_syscall),
#endif
#ifdef __NR_exit
    ALCOVE_DEFINE(__NR_exit),
#endif
#ifdef __NR_fork
    ALCOVE_DEFINE(__NR_fork),
#endif
#ifdef __NR_read
    ALCOVE_DEFINE(__NR_read),
#endif
#ifdef __NR_write
    ALCOVE_DEFINE(__NR_write),
#endif
#ifdef __NR_open
    ALCOVE_DEFINE(__NR_open),
#endif
#ifdef __NR_close
    ALCOVE_DEFINE(__NR_close),
#endif
#ifdef __NR_creat
    ALCOVE_DEFINE(__NR_creat),
#endif
#ifdef __NR_link
    ALCOVE_DEFINE(__NR_link),
#endif
#ifdef __NR_unlink
    ALCOVE_DEFINE(__NR_unlink),
#endif
#ifdef __NR_execve
    ALCOVE_DEFINE(__NR_execve),
#endif
#ifdef __NR_chdir
    ALCOVE_DEFINE(__NR_chdir),
#endif
#ifdef __NR_time
    ALCOVE_DEFINE(__NR_time),
#endif
#ifdef __NR_mknod
    ALCOVE_DEFINE(__NR_mknod),
#endif
#ifdef __NR_chmod
    ALCOVE_DEFINE(__NR_chmod),
#endif
#ifdef __NR_lchown
    ALCOVE_DEFINE(__NR_lchown),
#endif
#ifdef __NR_lseek
    ALCOVE_DEFINE(__NR_lseek),
#endif
#ifdef __NR_getpid
    ALCOVE_DEFINE(__NR_getpid),
#endif
#ifdef __NR_mount
    ALCOVE_DEFINE(__NR_mount),
#endif
#ifdef __NR_umount
    ALCOVE_DEFINE(__NR_umount),
#endif
#ifdef __NR_setuid
    ALCOVE_DEFINE(__NR_setuid),
#endif
#ifdef __NR_getuid
    ALCOVE_DEFINE(__NR_getuid),
#endif
#ifdef __NR_stime
    ALCOVE_DEFINE(__NR_stime),
#endif
#ifdef __NR_ptrace
    ALCOVE_DEFINE(__NR_ptrace),
#endif
#ifdef __NR_alarm
    ALCOVE_DEFINE(__NR_alarm),
#endif
#ifdef __NR_pause
    ALCOVE_DEFINE(__NR_pause),
#endif
#ifdef __NR_utime
    ALCOVE_DEFINE(__NR_utime),
#endif
#ifdef __NR_access
    ALCOVE_DEFINE(__NR_access),
#endif
#ifdef __NR_nice
    ALCOVE_DEFINE(__NR_nice),
#endif
#ifdef __NR_sync
    ALCOVE_DEFINE(__NR_sync),
#endif
#ifdef __NR_kill
    ALCOVE_DEFINE(__NR_kill),
#endif
#ifdef __NR_rename
    ALCOVE_DEFINE(__NR_rename),
#endif
#ifdef __NR_mkdir
    ALCOVE_DEFINE(__NR_mkdir),
#endif
#ifdef __NR_rmdir
    ALCOVE_DEFINE(__NR_rmdir),
#endif
#ifdef __NR_dup
    ALCOVE_DEFINE(__NR_dup),
#endif
#ifdef __NR_pipe
    ALCOVE_DEFINE(__NR_pipe),
#endif
#ifdef __NR_times
    ALCOVE_DEFINE(__NR_times),
#endif
#ifdef __NR_brk
    ALCOVE_DEFINE(__NR_brk),
#endif
#ifdef __NR_setgid
    ALCOVE_DEFINE(__NR_setgid),
#endif
#ifdef __NR_getgid
    ALCOVE_DEFINE(__NR_getgid),
#endif
#ifdef __NR_geteuid
    ALCOVE_DEFINE(__NR_geteuid),
#endif
#ifdef __NR_getegid
    ALCOVE_DEFINE(__NR_getegid),
#endif
#ifdef __NR_acct
    ALCOVE_DEFINE(__NR_acct),
#endif
#ifdef __NR_umount2
    ALCOVE_DEFINE(__NR_umount2),
#endif
#ifdef __NR_ioctl
    ALCOVE_DEFINE(__NR_ioctl),
#endif
#ifdef __NR_fcntl
    ALCOVE_DEFINE(__NR_fcntl),
#endif
#ifdef __NR_setpgid
    ALCOVE_DEFINE(__NR_setpgid),
#endif
#ifdef __NR_umask
    ALCOVE_DEFINE(__NR_umask),
#endif
#ifdef __NR_chroot
    ALCOVE_DEFINE(__NR_chroot),
#endif
#ifdef __NR_ustat
    ALCOVE_DEFINE(__NR_ustat),
#endif
#ifdef __NR_dup2
    ALCOVE_DEFINE(__NR_dup2),
#endif
#ifdef __NR_getppid
    ALCOVE_DEFINE(__NR_getppid),
#endif
#ifdef __NR_getpgrp
    ALCOVE_DEFINE(__NR_getpgrp),
#endif
#ifdef __NR_setsid
    ALCOVE_DEFINE(__NR_setsid),
#endif
#ifdef __NR_sigaction
    ALCOVE_DEFINE(__NR_sigaction),
#endif
#ifdef __NR_setreuid
    ALCOVE_DEFINE(__NR_setreuid),
#endif
#ifdef __NR_setregid
    ALCOVE_DEFINE(__NR_setregid),
#endif
#ifdef __NR_sigsuspend
    ALCOVE_DEFINE(__NR_sigsuspend),
#endif
#ifdef __NR_sigpending
    ALCOVE_DEFINE(__NR_sigpending),
#endif
#ifdef __NR_sethostname
    ALCOVE_DEFINE(__NR_sethostname),
#endif
#ifdef __NR_setrlimit
    ALCOVE_DEFINE(__NR_setrlimit),
#endif
#ifdef __NR_getrlimit
    ALCOVE_DEFINE(__NR_getrlimit),
#endif
#ifdef __NR_getrusage
    ALCOVE_DEFINE(__NR_getrusage),
#endif
#ifdef __NR_gettimeofday
    ALCOVE_DEFINE(__NR_gettimeofday),
#endif
#ifdef __NR_settimeofday
    ALCOVE_DEFINE(__NR_settimeofday),
#endif
#ifdef __NR_getgroups
    ALCOVE_DEFINE(__NR_getgroups),
#endif
#ifdef __NR_setgroups
    ALCOVE_DEFINE(__NR_setgroups),
#endif
#ifdef __NR_select
    ALCOVE_DEFINE(__NR_select),
#endif
#ifdef __NR_symlink
    ALCOVE_DEFINE(__NR_symlink),
#endif
#ifdef __NR_readlink
    ALCOVE_DEFINE(__NR_readlink),
#endif
#ifdef __NR_uselib
    ALCOVE_DEFINE(__NR_uselib),
#endif
#ifdef __NR_swapon
    ALCOVE_DEFINE(__NR_swapon),
#endif
#ifdef __NR_reboot
    ALCOVE_DEFINE(__NR_reboot),
#endif
#ifdef __NR_readdir
    ALCOVE_DEFINE(__NR_readdir),
#endif
#ifdef __NR_mmap
    ALCOVE_DEFINE(__NR_mmap),
#endif
#ifdef __NR_munmap
    ALCOVE_DEFINE(__NR_munmap),
#endif
#ifdef __NR_truncate
    ALCOVE_DEFINE(__NR_truncate),
#endif
#ifdef __NR_ftruncate
    ALCOVE_DEFINE(__NR_ftruncate),
#endif
#ifdef __NR_fchmod
    ALCOVE_DEFINE(__NR_fchmod),
#endif
#ifdef __NR_fchown
    ALCOVE_DEFINE(__NR_fchown),
#endif
#ifdef __NR_getpriority
    ALCOVE_DEFINE(__NR_getpriority),
#endif
#ifdef __NR_setpriority
    ALCOVE_DEFINE(__NR_setpriority),
#endif
#ifdef __NR_statfs
    ALCOVE_DEFINE(__NR_statfs),
#endif
#ifdef __NR_fstatfs
    ALCOVE_DEFINE(__NR_fstatfs),
#endif
#ifdef __NR_socketcall
    ALCOVE_DEFINE(__NR_socketcall),
#endif
#ifdef __NR_syslog
    ALCOVE_DEFINE(__NR_syslog),
#endif
#ifdef __NR_setitimer
    ALCOVE_DEFINE(__NR_setitimer),
#endif
#ifdef __NR_getitimer
    ALCOVE_DEFINE(__NR_getitimer),
#endif
#ifdef __NR_stat
    ALCOVE_DEFINE(__NR_stat),
#endif
#ifdef __NR_lstat
    ALCOVE_DEFINE(__NR_lstat),
#endif
#ifdef __NR_fstat
    ALCOVE_DEFINE(__NR_fstat),
#endif
#ifdef __NR_vhangup
    ALCOVE_DEFINE(__NR_vhangup),
#endif
#ifdef __NR_syscall
    ALCOVE_DEFINE(__NR_syscall),
#endif
#ifdef __NR_wait4
    ALCOVE_DEFINE(__NR_wait4),
#endif
#ifdef __NR_swapoff
    ALCOVE_DEFINE(__NR_swapoff),
#endif
#ifdef __NR_sysinfo
    ALCOVE_DEFINE(__NR_sysinfo),
#endif
#ifdef __NR_ipc
    ALCOVE_DEFINE(__NR_ipc),
#endif
#ifdef __NR_fsync
    ALCOVE_DEFINE(__NR_fsync),
#endif
#ifdef __NR_sigreturn
    ALCOVE_DEFINE(__NR_sigreturn),
#endif
#ifdef __NR_clone
    ALCOVE_DEFINE(__NR_clone),
#endif
#ifdef __NR_setdomainname
    ALCOVE_DEFINE(__NR_setdomainname),
#endif
#ifdef __NR_uname
    ALCOVE_DEFINE(__NR_uname),
#endif
#ifdef __NR_adjtimex
    ALCOVE_DEFINE(__NR_adjtimex),
#endif
#ifdef __NR_mprotect
    ALCOVE_DEFINE(__NR_mprotect),
#endif
#ifdef __NR_sigprocmask
    ALCOVE_DEFINE(__NR_sigprocmask),
#endif
#ifdef __NR_init_module
    ALCOVE_DEFINE(__NR_init_module),
#endif
#ifdef __NR_delete_module
    ALCOVE_DEFINE(__NR_delete_module),
#endif
#ifdef __NR_quotactl
    ALCOVE_DEFINE(__NR_quotactl),
#endif
#ifdef __NR_getpgid
    ALCOVE_DEFINE(__NR_getpgid),
#endif
#ifdef __NR_fchdir
    ALCOVE_DEFINE(__NR_fchdir),
#endif
#ifdef __NR_bdflush
    ALCOVE_DEFINE(__NR_bdflush),
#endif
#ifdef __NR_sysfs
    ALCOVE_DEFINE(__NR_sysfs),
#endif
#ifdef __NR_personality
    ALCOVE_DEFINE(__NR_personality),
#endif
#ifdef __NR_setfsuid
    ALCOVE_DEFINE(__NR_setfsuid),
#endif
#ifdef __NR_setfsgid
    ALCOVE_DEFINE(__NR_setfsgid),
#endif
#ifdef __NR__llseek
    ALCOVE_DEFINE(__NR__llseek),
#endif
#ifdef __NR_getdents
    ALCOVE_DEFINE(__NR_getdents),
#endif
#ifdef __NR__newselect
    ALCOVE_DEFINE(__NR__newselect),
#endif
#ifdef __NR_flock
    ALCOVE_DEFINE(__NR_flock),
#endif
#ifdef __NR_msync
    ALCOVE_DEFINE(__NR_msync),
#endif
#ifdef __NR_readv
    ALCOVE_DEFINE(__NR_readv),
#endif
#ifdef __NR_writev
    ALCOVE_DEFINE(__NR_writev),
#endif
#ifdef __NR_getsid
    ALCOVE_DEFINE(__NR_getsid),
#endif
#ifdef __NR_fdatasync
    ALCOVE_DEFINE(__NR_fdatasync),
#endif
#ifdef __NR__sysctl
    ALCOVE_DEFINE(__NR__sysctl),
#endif
#ifdef __NR_mlock
    ALCOVE_DEFINE(__NR_mlock),
#endif
#ifdef __NR_munlock
    ALCOVE_DEFINE(__NR_munlock),
#endif
#ifdef __NR_mlockall
    ALCOVE_DEFINE(__NR_mlockall),
#endif
#ifdef __NR_munlockall
    ALCOVE_DEFINE(__NR_munlockall),
#endif
#ifdef __NR_sched_setparam
    ALCOVE_DEFINE(__NR_sched_setparam),
#endif
#ifdef __NR_sched_getparam
    ALCOVE_DEFINE(__NR_sched_getparam),
#endif
#ifdef __NR_sched_setscheduler
    ALCOVE_DEFINE(__NR_sched_setscheduler),
#endif
#ifdef __NR_sched_getscheduler
    ALCOVE_DEFINE(__NR_sched_getscheduler),
#endif
#ifdef __NR_sched_yield
    ALCOVE_DEFINE(__NR_sched_yield),
#endif
#ifdef __NR_sched_get_priority_max
    ALCOVE_DEFINE(__NR_sched_get_priority_max),
#endif
#ifdef __NR_sched_get_priority_min
    ALCOVE_DEFINE(__NR_sched_get_priority_min),
#endif
#ifdef __NR_sched_rr_get_interval
    ALCOVE_DEFINE(__NR_sched_rr_get_interval),
#endif
#ifdef __NR_nanosleep
    ALCOVE_DEFINE(__NR_nanosleep),
#endif
#ifdef __NR_mremap
    ALCOVE_DEFINE(__NR_mremap),
#endif
#ifdef __NR_setresuid
    ALCOVE_DEFINE(__NR_setresuid),
#endif
#ifdef __NR_getresuid
    ALCOVE_DEFINE(__NR_getresuid),
#endif
#ifdef __NR_poll
    ALCOVE_DEFINE(__NR_poll),
#endif
#ifdef __NR_nfsservctl
    ALCOVE_DEFINE(__NR_nfsservctl),
#endif
#ifdef __NR_setresgid
    ALCOVE_DEFINE(__NR_setresgid),
#endif
#ifdef __NR_getresgid
    ALCOVE_DEFINE(__NR_getresgid),
#endif
#ifdef __NR_prctl
    ALCOVE_DEFINE(__NR_prctl),
#endif
#ifdef __NR_rt_sigreturn
    ALCOVE_DEFINE(__NR_rt_sigreturn),
#endif
#ifdef __NR_rt_sigaction
    ALCOVE_DEFINE(__NR_rt_sigaction),
#endif
#ifdef __NR_rt_sigprocmask
    ALCOVE_DEFINE(__NR_rt_sigprocmask),
#endif
#ifdef __NR_rt_sigpending
    ALCOVE_DEFINE(__NR_rt_sigpending),
#endif
#ifdef __NR_rt_sigtimedwait
    ALCOVE_DEFINE(__NR_rt_sigtimedwait),
#endif
#ifdef __NR_rt_sigqueueinfo
    ALCOVE_DEFINE(__NR_rt_sigqueueinfo),
#endif
#ifdef __NR_rt_sigsuspend
    ALCOVE_DEFINE(__NR_rt_sigsuspend),
#endif
#ifdef __NR_pread64
    ALCOVE_DEFINE(__NR_pread64),
#endif
#ifdef __NR_pwrite64
    ALCOVE_DEFINE(__NR_pwrite64),
#endif
#ifdef __NR_chown
    ALCOVE_DEFINE(__NR_chown),
#endif
#ifdef __NR_getcwd
    ALCOVE_DEFINE(__NR_getcwd),
#endif
#ifdef __NR_capget
    ALCOVE_DEFINE(__NR_capget),
#endif
#ifdef __NR_capset
    ALCOVE_DEFINE(__NR_capset),
#endif
#ifdef __NR_sigaltstack
    ALCOVE_DEFINE(__NR_sigaltstack),
#endif
#ifdef __NR_sendfile
    ALCOVE_DEFINE(__NR_sendfile),
#endif
#ifdef __NR_vfork
    ALCOVE_DEFINE(__NR_vfork),
#endif
#ifdef __NR_ugetrlimit
    ALCOVE_DEFINE(__NR_ugetrlimit),
#endif
#ifdef __NR_mmap2
    ALCOVE_DEFINE(__NR_mmap2),
#endif
#ifdef __NR_truncate64
    ALCOVE_DEFINE(__NR_truncate64),
#endif
#ifdef __NR_ftruncate64
    ALCOVE_DEFINE(__NR_ftruncate64),
#endif
#ifdef __NR_stat64
    ALCOVE_DEFINE(__NR_stat64),
#endif
#ifdef __NR_lstat64
    ALCOVE_DEFINE(__NR_lstat64),
#endif
#ifdef __NR_fstat64
    ALCOVE_DEFINE(__NR_fstat64),
#endif
#ifdef __NR_lchown32
    ALCOVE_DEFINE(__NR_lchown32),
#endif
#ifdef __NR_getuid32
    ALCOVE_DEFINE(__NR_getuid32),
#endif
#ifdef __NR_getgid32
    ALCOVE_DEFINE(__NR_getgid32),
#endif
#ifdef __NR_geteuid32
    ALCOVE_DEFINE(__NR_geteuid32),
#endif
#ifdef __NR_getegid32
    ALCOVE_DEFINE(__NR_getegid32),
#endif
#ifdef __NR_setreuid32
    ALCOVE_DEFINE(__NR_setreuid32),
#endif
#ifdef __NR_setregid32
    ALCOVE_DEFINE(__NR_setregid32),
#endif
#ifdef __NR_getgroups32
    ALCOVE_DEFINE(__NR_getgroups32),
#endif
#ifdef __NR_setgroups32
    ALCOVE_DEFINE(__NR_setgroups32),
#endif
#ifdef __NR_fchown32
    ALCOVE_DEFINE(__NR_fchown32),
#endif
#ifdef __NR_setresuid32
    ALCOVE_DEFINE(__NR_setresuid32),
#endif
#ifdef __NR_getresuid32
    ALCOVE_DEFINE(__NR_getresuid32),
#endif
#ifdef __NR_setresgid32
    ALCOVE_DEFINE(__NR_setresgid32),
#endif
#ifdef __NR_getresgid32
    ALCOVE_DEFINE(__NR_getresgid32),
#endif
#ifdef __NR_chown32
    ALCOVE_DEFINE(__NR_chown32),
#endif
#ifdef __NR_setuid32
    ALCOVE_DEFINE(__NR_setuid32),
#endif
#ifdef __NR_setgid32
    ALCOVE_DEFINE(__NR_setgid32),
#endif
#ifdef __NR_setfsuid32
    ALCOVE_DEFINE(__NR_setfsuid32),
#endif
#ifdef __NR_setfsgid32
    ALCOVE_DEFINE(__NR_setfsgid32),
#endif
#ifdef __NR_getdents64
    ALCOVE_DEFINE(__NR_getdents64),
#endif
#ifdef __NR_pivot_root
    ALCOVE_DEFINE(__NR_pivot_root),
#endif
#ifdef __NR_mincore
    ALCOVE_DEFINE(__NR_mincore),
#endif
#ifdef __NR_madvise
    ALCOVE_DEFINE(__NR_madvise),
#endif
#ifdef __NR_fcntl64
    ALCOVE_DEFINE(__NR_fcntl64),
#endif
#ifdef __NR_gettid
    ALCOVE_DEFINE(__NR_gettid),
#endif
#ifdef __NR_readahead
    ALCOVE_DEFINE(__NR_readahead),
#endif
#ifdef __NR_setxattr
    ALCOVE_DEFINE(__NR_setxattr),
#endif
#ifdef __NR_lsetxattr
    ALCOVE_DEFINE(__NR_lsetxattr),
#endif
#ifdef __NR_fsetxattr
    ALCOVE_DEFINE(__NR_fsetxattr),
#endif
#ifdef __NR_getxattr
    ALCOVE_DEFINE(__NR_getxattr),
#endif
#ifdef __NR_lgetxattr
    ALCOVE_DEFINE(__NR_lgetxattr),
#endif
#ifdef __NR_fgetxattr
    ALCOVE_DEFINE(__NR_fgetxattr),
#endif
#ifdef __NR_listxattr
    ALCOVE_DEFINE(__NR_listxattr),
#endif
#ifdef __NR_llistxattr
    ALCOVE_DEFINE(__NR_llistxattr),
#endif
#ifdef __NR_flistxattr
    ALCOVE_DEFINE(__NR_flistxattr),
#endif
#ifdef __NR_removexattr
    ALCOVE_DEFINE(__NR_removexattr),
#endif
#ifdef __NR_lremovexattr
    ALCOVE_DEFINE(__NR_lremovexattr),
#endif
#ifdef __NR_fremovexattr
    ALCOVE_DEFINE(__NR_fremovexattr),
#endif
#ifdef __NR_tkill
    ALCOVE_DEFINE(__NR_tkill),
#endif
#ifdef __NR_sendfile64
    ALCOVE_DEFINE(__NR_sendfile64),
#endif
#ifdef __NR_futex
    ALCOVE_DEFINE(__NR_futex),
#endif
#ifdef __NR_sched_setaffinity
    ALCOVE_DEFINE(__NR_sched_setaffinity),
#endif
#ifdef __NR_sched_getaffinity
    ALCOVE_DEFINE(__NR_sched_getaffinity),
#endif
#ifdef __NR_io_setup
    ALCOVE_DEFINE(__NR_io_setup),
#endif
#ifdef __NR_io_destroy
    ALCOVE_DEFINE(__NR_io_destroy),
#endif
#ifdef __NR_io_getevents
    ALCOVE_DEFINE(__NR_io_getevents),
#endif
#ifdef __NR_io_submit
    ALCOVE_DEFINE(__NR_io_submit),
#endif
#ifdef __NR_io_cancel
    ALCOVE_DEFINE(__NR_io_cancel),
#endif
#ifdef __NR_exit_group
    ALCOVE_DEFINE(__NR_exit_group),
#endif
#ifdef __NR_lookup_dcookie
    ALCOVE_DEFINE(__NR_lookup_dcookie),
#endif
#ifdef __NR_epoll_create
    ALCOVE_DEFINE(__NR_epoll_create),
#endif
#ifdef __NR_epoll_ctl
    ALCOVE_DEFINE(__NR_epoll_ctl),
#endif
#ifdef __NR_epoll_wait
    ALCOVE_DEFINE(__NR_epoll_wait),
#endif
#ifdef __NR_remap_file_pages
    ALCOVE_DEFINE(__NR_remap_file_pages),
#endif
#ifdef __NR_set_tid_address
    ALCOVE_DEFINE(__NR_set_tid_address),
#endif
#ifdef __NR_timer_create
    ALCOVE_DEFINE(__NR_timer_create),
#endif
#ifdef __NR_timer_settime
    ALCOVE_DEFINE(__NR_timer_settime),
#endif
#ifdef __NR_timer_gettime
    ALCOVE_DEFINE(__NR_timer_gettime),
#endif
#ifdef __NR_timer_getoverrun
    ALCOVE_DEFINE(__NR_timer_getoverrun),
#endif
#ifdef __NR_timer_delete
    ALCOVE_DEFINE(__NR_timer_delete),
#endif
#ifdef __NR_clock_settime
    ALCOVE_DEFINE(__NR_clock_settime),
#endif
#ifdef __NR_clock_gettime
    ALCOVE_DEFINE(__NR_clock_gettime),
#endif
#ifdef __NR_clock_getres
    ALCOVE_DEFINE(__NR_clock_getres),
#endif
#ifdef __NR_clock_nanosleep
    ALCOVE_DEFINE(__NR_clock_nanosleep),
#endif
#ifdef __NR_statfs64
    ALCOVE_DEFINE(__NR_statfs64),
#endif
#ifdef __NR_fstatfs64
    ALCOVE_DEFINE(__NR_fstatfs64),
#endif
#ifdef __NR_tgkill
    ALCOVE_DEFINE(__NR_tgkill),
#endif
#ifdef __NR_utimes
    ALCOVE_DEFINE(__NR_utimes),
#endif
#ifdef __NR_arm_fadvise64_64
    ALCOVE_DEFINE(__NR_arm_fadvise64_64),
#endif
#ifdef __NR_pciconfig_iobase
    ALCOVE_DEFINE(__NR_pciconfig_iobase),
#endif
#ifdef __NR_pciconfig_read
    ALCOVE_DEFINE(__NR_pciconfig_read),
#endif
#ifdef __NR_pciconfig_write
    ALCOVE_DEFINE(__NR_pciconfig_write),
#endif
#ifdef __NR_mq_open
    ALCOVE_DEFINE(__NR_mq_open),
#endif
#ifdef __NR_mq_unlink
    ALCOVE_DEFINE(__NR_mq_unlink),
#endif
#ifdef __NR_mq_timedsend
    ALCOVE_DEFINE(__NR_mq_timedsend),
#endif
#ifdef __NR_mq_timedreceive
    ALCOVE_DEFINE(__NR_mq_timedreceive),
#endif
#ifdef __NR_mq_notify
    ALCOVE_DEFINE(__NR_mq_notify),
#endif
#ifdef __NR_mq_getsetattr
    ALCOVE_DEFINE(__NR_mq_getsetattr),
#endif
#ifdef __NR_waitid
    ALCOVE_DEFINE(__NR_waitid),
#endif
#ifdef __NR_socket
    ALCOVE_DEFINE(__NR_socket),
#endif
#ifdef __NR_bind
    ALCOVE_DEFINE(__NR_bind),
#endif
#ifdef __NR_connect
    ALCOVE_DEFINE(__NR_connect),
#endif
#ifdef __NR_listen
    ALCOVE_DEFINE(__NR_listen),
#endif
#ifdef __NR_accept
    ALCOVE_DEFINE(__NR_accept),
#endif
#ifdef __NR_getsockname
    ALCOVE_DEFINE(__NR_getsockname),
#endif
#ifdef __NR_getpeername
    ALCOVE_DEFINE(__NR_getpeername),
#endif
#ifdef __NR_socketpair
    ALCOVE_DEFINE(__NR_socketpair),
#endif
#ifdef __NR_send
    ALCOVE_DEFINE(__NR_send),
#endif
#ifdef __NR_sendto
    ALCOVE_DEFINE(__NR_sendto),
#endif
#ifdef __NR_recv
    ALCOVE_DEFINE(__NR_recv),
#endif
#ifdef __NR_recvfrom
    ALCOVE_DEFINE(__NR_recvfrom),
#endif
#ifdef __NR_shutdown
    ALCOVE_DEFINE(__NR_shutdown),
#endif
#ifdef __NR_setsockopt
    ALCOVE_DEFINE(__NR_setsockopt),
#endif
#ifdef __NR_getsockopt
    ALCOVE_DEFINE(__NR_getsockopt),
#endif
#ifdef __NR_sendmsg
    ALCOVE_DEFINE(__NR_sendmsg),
#endif
#ifdef __NR_recvmsg
    ALCOVE_DEFINE(__NR_recvmsg),
#endif
#ifdef __NR_semop
    ALCOVE_DEFINE(__NR_semop),
#endif
#ifdef __NR_semget
    ALCOVE_DEFINE(__NR_semget),
#endif
#ifdef __NR_semctl
    ALCOVE_DEFINE(__NR_semctl),
#endif
#ifdef __NR_msgsnd
    ALCOVE_DEFINE(__NR_msgsnd),
#endif
#ifdef __NR_msgrcv
    ALCOVE_DEFINE(__NR_msgrcv),
#endif
#ifdef __NR_msgget
    ALCOVE_DEFINE(__NR_msgget),
#endif
#ifdef __NR_msgctl
    ALCOVE_DEFINE(__NR_msgctl),
#endif
#ifdef __NR_shmat
    ALCOVE_DEFINE(__NR_shmat),
#endif
#ifdef __NR_shmdt
    ALCOVE_DEFINE(__NR_shmdt),
#endif
#ifdef __NR_shmget
    ALCOVE_DEFINE(__NR_shmget),
#endif
#ifdef __NR_shmctl
    ALCOVE_DEFINE(__NR_shmctl),
#endif
#ifdef __NR_add_key
    ALCOVE_DEFINE(__NR_add_key),
#endif
#ifdef __NR_request_key
    ALCOVE_DEFINE(__NR_request_key),
#endif
#ifdef __NR_keyctl
    ALCOVE_DEFINE(__NR_keyctl),
#endif
#ifdef __NR_semtimedop
    ALCOVE_DEFINE(__NR_semtimedop),
#endif
#ifdef __NR_vserver
    ALCOVE_DEFINE(__NR_vserver),
#endif
#ifdef __NR_ioprio_set
    ALCOVE_DEFINE(__NR_ioprio_set),
#endif
#ifdef __NR_ioprio_get
    ALCOVE_DEFINE(__NR_ioprio_get),
#endif
#ifdef __NR_inotify_init
    ALCOVE_DEFINE(__NR_inotify_init),
#endif
#ifdef __NR_inotify_add_watch
    ALCOVE_DEFINE(__NR_inotify_add_watch),
#endif
#ifdef __NR_inotify_rm_watch
    ALCOVE_DEFINE(__NR_inotify_rm_watch),
#endif
#ifdef __NR_mbind
    ALCOVE_DEFINE(__NR_mbind),
#endif
#ifdef __NR_get_mempolicy
    ALCOVE_DEFINE(__NR_get_mempolicy),
#endif
#ifdef __NR_set_mempolicy
    ALCOVE_DEFINE(__NR_set_mempolicy),
#endif
#ifdef __NR_openat
    ALCOVE_DEFINE(__NR_openat),
#endif
#ifdef __NR_mkdirat
    ALCOVE_DEFINE(__NR_mkdirat),
#endif
#ifdef __NR_mknodat
    ALCOVE_DEFINE(__NR_mknodat),
#endif
#ifdef __NR_fchownat
    ALCOVE_DEFINE(__NR_fchownat),
#endif
#ifdef __NR_futimesat
    ALCOVE_DEFINE(__NR_futimesat),
#endif
#ifdef __NR_fstatat64
    ALCOVE_DEFINE(__NR_fstatat64),
#endif
#ifdef __NR_unlinkat
    ALCOVE_DEFINE(__NR_unlinkat),
#endif
#ifdef __NR_renameat
    ALCOVE_DEFINE(__NR_renameat),
#endif
#ifdef __NR_linkat
    ALCOVE_DEFINE(__NR_linkat),
#endif
#ifdef __NR_symlinkat
    ALCOVE_DEFINE(__NR_symlinkat),
#endif
#ifdef __NR_readlinkat
    ALCOVE_DEFINE(__NR_readlinkat),
#endif
#ifdef __NR_fchmodat
    ALCOVE_DEFINE(__NR_fchmodat),
#endif
#ifdef __NR_faccessat
    ALCOVE_DEFINE(__NR_faccessat),
#endif
#ifdef __NR_pselect6
    ALCOVE_DEFINE(__NR_pselect6),
#endif
#ifdef __NR_ppoll
    ALCOVE_DEFINE(__NR_ppoll),
#endif
#ifdef __NR_unshare
    ALCOVE_DEFINE(__NR_unshare),
#endif
#ifdef __NR_set_robust_list
    ALCOVE_DEFINE(__NR_set_robust_list),
#endif
#ifdef __NR_get_robust_list
    ALCOVE_DEFINE(__NR_get_robust_list),
#endif
#ifdef __NR_splice
    ALCOVE_DEFINE(__NR_splice),
#endif
#ifdef __NR_arm_sync_file_range
    ALCOVE_DEFINE(__NR_arm_sync_file_range),
#endif
#ifdef __NR_sync_file_range2
    ALCOVE_DEFINE(__NR_sync_file_range2),
#endif
#ifdef __NR_tee
    ALCOVE_DEFINE(__NR_tee),
#endif
#ifdef __NR_vmsplice
    ALCOVE_DEFINE(__NR_vmsplice),
#endif
#ifdef __NR_move_pages
    ALCOVE_DEFINE(__NR_move_pages),
#endif
#ifdef __NR_getcpu
    ALCOVE_DEFINE(__NR_getcpu),
#endif
#ifdef __NR_epoll_pwait
    ALCOVE_DEFINE(__NR_epoll_pwait),
#endif
#ifdef __NR_kexec_load
    ALCOVE_DEFINE(__NR_kexec_load),
#endif
#ifdef __NR_utimensat
    ALCOVE_DEFINE(__NR_utimensat),
#endif
#ifdef __NR_signalfd
    ALCOVE_DEFINE(__NR_signalfd),
#endif
#ifdef __NR_timerfd_create
    ALCOVE_DEFINE(__NR_timerfd_create),
#endif
#ifdef __NR_eventfd
    ALCOVE_DEFINE(__NR_eventfd),
#endif
#ifdef __NR_fallocate
    ALCOVE_DEFINE(__NR_fallocate),
#endif
#ifdef __NR_timerfd_settime
    ALCOVE_DEFINE(__NR_timerfd_settime),
#endif
#ifdef __NR_timerfd_gettime
    ALCOVE_DEFINE(__NR_timerfd_gettime),
#endif
#ifdef __NR_signalfd4
    ALCOVE_DEFINE(__NR_signalfd4),
#endif
#ifdef __NR_eventfd2
    ALCOVE_DEFINE(__NR_eventfd2),
#endif
#ifdef __NR_epoll_create1
    ALCOVE_DEFINE(__NR_epoll_create1),
#endif
#ifdef __NR_dup3
    ALCOVE_DEFINE(__NR_dup3),
#endif
#ifdef __NR_pipe2
    ALCOVE_DEFINE(__NR_pipe2),
#endif
#ifdef __NR_inotify_init1
    ALCOVE_DEFINE(__NR_inotify_init1),
#endif
#ifdef __NR_preadv
    ALCOVE_DEFINE(__NR_preadv),
#endif
#ifdef __NR_pwritev
    ALCOVE_DEFINE(__NR_pwritev),
#endif
#ifdef __NR_rt_tgsigqueueinfo
    ALCOVE_DEFINE(__NR_rt_tgsigqueueinfo),
#endif
#ifdef __NR_perf_event_open
    ALCOVE_DEFINE(__NR_perf_event_open),
#endif
#ifdef __NR_recvmmsg
    ALCOVE_DEFINE(__NR_recvmmsg),
#endif
#ifdef __NR_accept4
    ALCOVE_DEFINE(__NR_accept4),
#endif
#ifdef __NR_fanotify_init
    ALCOVE_DEFINE(__NR_fanotify_init),
#endif
#ifdef __NR_fanotify_mark
    ALCOVE_DEFINE(__NR_fanotify_mark),
#endif
#ifdef __NR_prlimit64
    ALCOVE_DEFINE(__NR_prlimit64),
#endif
#ifdef __NR_name_to_handle_at
    ALCOVE_DEFINE(__NR_name_to_handle_at),
#endif
#ifdef __NR_open_by_handle_at
    ALCOVE_DEFINE(__NR_open_by_handle_at),
#endif
#ifdef __NR_clock_adjtime
    ALCOVE_DEFINE(__NR_clock_adjtime),
#endif
#ifdef __NR_syncfs
    ALCOVE_DEFINE(__NR_syncfs),
#endif
#ifdef __NR_sendmmsg
    ALCOVE_DEFINE(__NR_sendmmsg),
#endif
#ifdef __NR_setns
    ALCOVE_DEFINE(__NR_setns),
#endif
#ifdef __NR_process_vm_readv
    ALCOVE_DEFINE(__NR_process_vm_readv),
#endif
#ifdef __NR_process_vm_writev
    ALCOVE_DEFINE(__NR_process_vm_writev),
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
