/* Copyright (c) 2015-2024, Michael Santos <michael.santos@gmail.com>
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
#include "alcove.h"

#include <poll.h>
#include <sys/wait.h>

#include <sys/stat.h>

enum {
  ALCOVE_MSG_STDIN = 0,
  ALCOVE_MSG_STDOUT,
  ALCOVE_MSG_STDERR,
  ALCOVE_MSG_PROXY,
  ALCOVE_MSG_CALL,
  ALCOVE_MSG_EVENT,
  ALCOVE_MSG_CTL,
  ALCOVE_MSG_PIPE,
};

#define ALCOVE_CHILD_EXEC -2

#define ALCOVE_MSG_TYPE(s)                                                     \
  ((s->fdctl == ALCOVE_CHILD_EXEC) ? ALCOVE_MSG_STDOUT : ALCOVE_MSG_PROXY)

#define ALCOVE_IOVEC_COUNT(_array) (sizeof(_array) / sizeof(_array[0]))

static int alcove_stdin(alcove_state_t *ap);
static ssize_t alcove_msg_call(alcove_state_t *ap, unsigned char *buf,
                               u_int16_t buflen);

static size_t alcove_proxy_hdr(unsigned char *hdr, size_t hdrlen,
                               u_int16_t type, pid_t pid, size_t buflen);
static size_t alcove_call_hdr(unsigned char *hdr, size_t hdrlen, u_int16_t type,
                              size_t buflen);

static ssize_t alcove_child_stdio(int fdin, u_int16_t depth, alcove_child_t *c,
                                  u_int16_t type);
static ssize_t alcove_call_reply(u_int16_t, char *, size_t);
static ssize_t alcove_call_spoof(pid_t pid, u_int16_t type, char *, size_t);

static int alcove_get_uint16(int fd, u_int16_t *val);
static ssize_t alcove_read(int, void *, ssize_t);
static ssize_t alcove_write(int fd, struct iovec *iov, int count);

static int exited_pid(alcove_state_t *ap, alcove_child_t *c, void *arg1,
                      void *arg2);
static int set_pid(alcove_state_t *ap, alcove_child_t *c, void *arg1,
                   void *arg2);
static int write_to_pid(alcove_state_t *ap, alcove_child_t *c, void *arg1,
                        void *arg2);
static int read_from_pid(alcove_state_t *ap, alcove_child_t *c, void *arg1,
                         void *arg2);
static int read_child_fdctl(alcove_state_t *ap, alcove_child_t *c);
static int read_child_stdout(alcove_state_t *ap, alcove_child_t *c);
static int read_child_stderr(alcove_state_t *ap, alcove_child_t *c);

static int alcove_handle_signal(alcove_state_t *ap);
static int alcove_signal_event(alcove_state_t *ap, siginfo_t *info);
static int signal_pid(alcove_state_t *ap, alcove_child_t *c, void *arg1,
                      void *arg2);

void alcove_event_init(alcove_state_t *ap) {
  int tlen;
  char t[MAXMSGLEN] = {0};

  /* process has exec'ed itself */
  tlen = alcove_mk_atom(t, sizeof(t), "ok");

  if (alcove_call_reply(ALCOVE_MSG_CALL, t, tlen) < 0)
    exit(EIO);

  alcove_event_loop(ap);
}

void alcove_event_loop(alcove_state_t *ap) {
  int i;
  struct pollfd *fds;

  (void)memcpy(ap->filter, ap->filter1, ALCOVE_NR_SIZE);
  (void)memset(ap->child, 0, sizeof(alcove_child_t) * ap->fdsetsize);

  fds = calloc(ap->maxfd, sizeof(struct pollfd));
  if (fds == NULL)
    exit(errno);

  for (;;) {
    if (ap->fdsetsize != ap->maxchild) {
      fds = recallocarray(fds, ap->maxfd, ALCOVE_NFD(ap->maxchild), sizeof(struct pollfd));
      if (fds == NULL)
        exit(errno);
      ap->maxfd = ALCOVE_NFD(ap->maxchild);

      ap->child = recallocarray(ap->child, ap->fdsetsize, ap->maxchild,
                                sizeof(alcove_child_t));
      if (ap->child == NULL)
        exit(errno);

      ap->fdsetsize = ap->maxchild;
    }

    for (i = 0; i < ap->maxfd; i++) {
      fds[i].fd = -1;
      fds[i].revents = 0;
    }

    fds[STDIN_FILENO].fd = STDIN_FILENO;
    fds[STDIN_FILENO].events = POLLIN;

    fds[ALCOVE_SIGREAD_FILENO].fd = ALCOVE_SIGREAD_FILENO;
    fds[ALCOVE_SIGREAD_FILENO].events = POLLIN;

    (void)pid_foreach(ap, 0, fds, NULL, pid_not_equal, set_pid);

    if (poll(fds, ap->maxfd, -1) < 0) {
      switch (errno) {
      case EINTR:
        continue;
      default:
        exit(errno);
      }
    }

    if (fds[STDIN_FILENO].revents & (POLLIN | POLLERR | POLLHUP | POLLNVAL)) {
      switch (alcove_stdin(ap)) {
      case 0:
        break;
      case 1:
        /* EOF */
        if (ap->signaloneof > 0) {
          (void)pid_foreach(ap, 0, NULL, NULL, pid_not_equal, signal_pid);
        }
        free(fds);
        return;
      case -1:
      default:
        if (ap->signaloneof > 0) {
          (void)pid_foreach(ap, 0, NULL, NULL, pid_not_equal, signal_pid);
        }
        exit(errno);
      }
    }

    if (fds[ALCOVE_SIGREAD_FILENO].revents &
        (POLLIN | POLLERR | POLLHUP | POLLNVAL)) {
      if (alcove_handle_signal(ap) < 0)
        exit(errno);
    }

    (void)pid_foreach(ap, 0, fds, NULL, pid_not_equal, read_from_pid);
  }
}

static int alcove_stdin(alcove_state_t *ap) {
  u_int16_t type;
  pid_t pid;
  unsigned char msg[MAXMSGLEN] = {0};
  unsigned char *buf = msg;
  u_int16_t buflen = 0;

  errno = 0;

  /*
   * Call:
   *  |length:2|call:2|command:2|arg:...|
   *
   * Stdin:
   *  |length:2|stdin:2|pid:4|data:...|
   *
   */

  /* total length, not including length header */
  if (alcove_get_uint16(STDIN_FILENO, &buflen) != sizeof(buflen)) {
    if (errno == 0)
      return 1;

    return -1;
  }

  if (alcove_read(STDIN_FILENO, buf, buflen) != buflen)
    return -1;

  type = get_int16(buf);
  buf += 2;
  buflen -= 2;

  switch (type) {
  case ALCOVE_MSG_CALL:
    if (alcove_msg_call(ap, buf, buflen) < 0)
      return -1;

    return 0;

  case ALCOVE_MSG_STDIN:
    if (buflen < sizeof(pid))
      return -1;

    pid = get_int32(buf);
    buf += 4;
    buflen -= 4;

    if ((pid <= 0) ||
        (pid_foreach(ap, pid, buf, &buflen, pid_equal, write_to_pid) == 1)) {
      int tlen = 0;
      char t[MAXMSGLEN] = {0};
      tlen = alcove_mk_atom(t, sizeof(t), "badpid");
      if (alcove_call_spoof(pid, ALCOVE_MSG_CTL, t, tlen) < 0)
        return -1;
    }

    return 0;

  default:
    return -1;
  }
}

static ssize_t alcove_msg_call(alcove_state_t *ap, unsigned char *buf,
                               u_int16_t buflen) {
  u_int16_t call;
  char reply[MAXMSGLEN] = {0};
  ssize_t rlen;

  if (buflen <= sizeof(call))
    return -1;

  call = get_int16(buf);
  buf += 2;

  rlen = alcove_call(ap, call, (const char *)buf, buflen, reply,
                     ALCOVE_MSGLEN(ap->depth, sizeof(reply)));

  /* Must crash on error. The port may have allocated memory or
   * performed some other destructive action.
   */
  if (rlen < 0)
    return -1;

  return alcove_call_reply(ALCOVE_MSG_CALL, reply, rlen);
}

static size_t alcove_proxy_hdr(unsigned char *hdr, size_t hdrlen,
                               u_int16_t type, pid_t pid, size_t buflen) {
  u_int16_t len;

  if (hdrlen < 8)
    return 0;

  put_int16(sizeof(type) + sizeof(pid) + buflen, hdr);
  len = 2;
  put_int16(type, hdr + len);
  len += 2;
  put_int32(pid, hdr + len);
  len += 4;

  return len;
}

static size_t alcove_call_hdr(unsigned char *hdr, size_t hdrlen, u_int16_t type,
                              size_t buflen) {
  u_int16_t len;

  if (hdrlen < 4)
    return 0;

  put_int16(sizeof(type) + buflen, hdr);
  len = 2;
  put_int16(type, hdr + len);
  len += 2;

  return len;
}

static ssize_t alcove_child_stdio(int fdin, u_int16_t depth, alcove_child_t *c,
                                  u_int16_t type) {
  struct iovec iov[2];

  ssize_t n = 0;
  unsigned char buf[MAXMSGLEN] = {0};
  unsigned char hdr[MAXHDRLEN] = {0};
  u_int16_t hdrlen = 0;
  size_t read_len = sizeof(hdrlen);

  int flags;
  int oerrno;

  /* XXX One message may be sent from a flowcontrolled process when it calls
   * XXX exec() because stdio for the process is added to the poll
   * XXX set before exec() is called.
   * XXX
   * XXX The return value (-2) is ignored.
   */
  if ((c->fdctl == ALCOVE_CHILD_EXEC) && (c->flowcontrol == 0) && !c->exited) {
    return -2;
  }

  /* If the child has called exec(), treat the data as a stream.
   *
   * Otherwise, read in the length header and do an exact read.
   */
  if ((c->fdctl == ALCOVE_CHILD_EXEC) || (type == ALCOVE_MSG_STDERR))
    read_len = ALCOVE_MSGLEN(depth, sizeof(buf));

  flags = fcntl(fdin, F_GETFL);
  if (flags < 0)
    return -1;

  if (fcntl(fdin, F_SETFL, flags | O_NONBLOCK) < 0)
    return -1;

  n = read(fdin, buf, read_len);
  oerrno = errno;

  if (fcntl(fdin, F_SETFL, flags) < 0)
    return -1;

  switch (n) {
  case 0:
    return 0;
  case -1:
    return (oerrno == EINTR || oerrno == EAGAIN) ? 0 : -1;
  default:
    break;
  }

  if ((c->fdctl == ALCOVE_CHILD_EXEC) && (c->flowcontrol > 0)) {
    c->flowcontrol--;
  }

  if ((c->fdctl != ALCOVE_CHILD_EXEC) && (type != ALCOVE_MSG_STDERR)) {
    if (n < 2)
      return -1;

    n = get_int16(buf);

    if (n > sizeof(buf) - 2)
      return -1;

    if (alcove_read(fdin, buf + 2, n) != n)
      return -1;

    n += 2;
  }

  hdrlen = alcove_proxy_hdr(hdr, sizeof(hdr), type, c->pid, n);

  if (hdrlen == 0)
    return -1;

  iov[0].iov_base = hdr;
  iov[0].iov_len = hdrlen;
  iov[1].iov_base = buf;
  iov[1].iov_len = n;

  return alcove_write(STDOUT_FILENO, iov, ALCOVE_IOVEC_COUNT(iov));
}

static ssize_t alcove_call_reply(u_int16_t type, char *buf, size_t len) {
  struct iovec iov[2];

  unsigned char hdr[MAXHDRLEN] = {0};
  u_int16_t hdrlen;

  hdrlen = alcove_call_hdr(hdr, sizeof(hdr), type, len);

  if (hdrlen == 0)
    return -1;

  iov[0].iov_base = hdr;
  iov[0].iov_len = hdrlen;
  iov[1].iov_base = buf;
  iov[1].iov_len = len;

  return alcove_write(STDOUT_FILENO, iov, ALCOVE_IOVEC_COUNT(iov));
}

static ssize_t alcove_call_spoof(pid_t pid, u_int16_t type, char *buf,
                                 size_t len) {
  struct iovec iov[3];

  unsigned char proxyhdr[MAXHDRLEN] = {0};
  u_int16_t proxyhdrlen;

  unsigned char callhdr[MAXHDRLEN] = {0};
  u_int16_t callhdrlen;

  callhdrlen = alcove_call_hdr(callhdr, sizeof(callhdr), type, len);

  if (callhdrlen == 0)
    return -1;

  proxyhdrlen = alcove_proxy_hdr(proxyhdr, sizeof(proxyhdr), ALCOVE_MSG_PROXY,
                                 pid, callhdrlen + len);

  if (proxyhdrlen == 0)
    return -1;

  iov[0].iov_base = proxyhdr;
  iov[0].iov_len = proxyhdrlen;
  iov[1].iov_base = callhdr;
  iov[1].iov_len = callhdrlen;
  iov[2].iov_base = buf;
  iov[2].iov_len = len;

  return alcove_write(STDOUT_FILENO, iov, ALCOVE_IOVEC_COUNT(iov));
}

static int alcove_get_uint16(int fd, u_int16_t *val) {
  u_int16_t buf = 0;
  ssize_t n;

  n = alcove_read(fd, &buf, sizeof(buf));

  if (n != sizeof(buf))
    return n;

  *val = ntohs(buf);
  return n;
}

static ssize_t alcove_read(int fd, void *buf, ssize_t len) {
  ssize_t i = 0;
  ssize_t got = 0;

  do {
    if ((i = read(fd, (char *)buf + got, len - got)) <= 0)
      return i;
    got += i;
  } while (got < len);

  return len;
}

static ssize_t alcove_write(int fd, struct iovec *iov, int count) {
  ssize_t written = 0;
  ssize_t n = 0;
  int offset = 0;

  do {
    iov[offset].iov_base = (char *)iov[offset].iov_base + written;
    iov[offset].iov_len -= written;

    written = writev(fd, iov + offset, count - offset);
    if (written <= 0)
      return written;

    n += written;

    for (; offset < count && written >= iov[offset].iov_len; offset++)
      written -= iov[offset].iov_len;

  } while (offset < count);

  return n;
}

static int exited_pid(alcove_state_t *ap, alcove_child_t *c, void *arg1,
                      void *arg2) {
  int *status = arg1;
  int index = 0;
  char t[MAXMSGLEN] = {0};

  UNUSED(arg2);

  c->exited = 1;

  /* Flush any pending reads and ensure messages are received in order */
  if (c->fdctl > -1)
    (void)read_child_fdctl(ap, c);
  if (c->fdout > -1)
    (void)read_child_stdout(ap, c);
  if (c->fderr > -1)
    (void)read_child_stderr(ap, c);

  if ((c->fdin >= 0) && (ap->opt & alcove_opt_stdin_closed)) {
    index = alcove_mk_atom(t, sizeof(t), "stdin_closed");
    if (alcove_call_spoof(c->pid, ALCOVE_MSG_CTL, t, index) < 0)
      return -1;
  }

  (void)close(c->fdin);
  c->fdin = -1;

  if (WIFEXITED(*status)) {
    if (ap->opt & alcove_opt_exit_status) {
      ALCOVE_TUPLE2(
          t, sizeof(t), &index, "exit_status",
          alcove_encode_long(t, sizeof(t), &index, WEXITSTATUS(*status)));

      if (alcove_call_spoof(c->pid, ALCOVE_MSG_EVENT, t, index))
        return -1;
    }
  }

  if (WIFSIGNALED(*status)) {
    if (ap->opt & alcove_opt_termsig) {
      ALCOVE_TUPLE2(
          t, sizeof(t), &index, "termsig",
          alcove_signal_name(t, sizeof(t), &index, WTERMSIG(*status)));

      if (alcove_call_spoof(c->pid, ALCOVE_MSG_EVENT, t, index) < 0)
        return -1;
    }
  }

  return 0;
}

static int set_pid(alcove_state_t *ap, alcove_child_t *c, void *arg1,
                   void *arg2) {
  struct pollfd *fds = arg1;

  UNUSED(ap);
  UNUSED(arg2);

  if (c->fdctl > -1) {
    fds[c->fdctl].fd = c->fdctl;
    fds[c->fdctl].events = POLLIN;
  }

  if (c->fdout > -1) {
    fds[c->fdout].fd = c->fdout;
    fds[c->fdout].events =
        (c->fdctl == ALCOVE_CHILD_EXEC && c->flowcontrol == 0) ? 0 : POLLIN;
  }

  if (c->fderr > -1) {
    fds[c->fderr].fd = c->fderr;
    fds[c->fderr].events =
        (c->fdctl == ALCOVE_CHILD_EXEC && c->flowcontrol == 0) ? 0 : POLLIN;
  }

  if (c->exited && c->fdout == -1 && c->fderr == -1 && c->fdctl < 0) {
    c->pid = 0;
    c->exited = 0;
  }

  return 1;
}

static int write_to_pid(alcove_state_t *ap, alcove_child_t *c, void *arg1,
                        void *arg2) {
  char *buf = arg1;
  u_int16_t *buflen = arg2;
  ssize_t n = 0;
  ssize_t written = 0;

  UNUSED(ap);

  if (c->fdin == -1)
    return -2;

  do {
    n = write(c->fdin, buf + written, *buflen - written);

    if (n <= 0) {
      switch (errno) {
      case EINTR:
        continue;
      case EAGAIN: {
        int tlen = 0;
        char t[MAXMSGLEN] = {0};
        tlen = alcove_mk_long(t, sizeof(t), written);
        if (alcove_call_spoof(c->pid, ALCOVE_MSG_PIPE, t, tlen) < 0)
          exit(errno);
        break;
      }
      default:
        break;
      }
      return n;
    }

    written += n;
  } while (written < *buflen);

  return 0;
}

static int read_from_pid(alcove_state_t *ap, alcove_child_t *c, void *arg1,
                         void *arg2) {
  struct pollfd *fds = arg1;

  UNUSED(arg2);

  if (c->fdctl > -1 &&
      (fds[c->fdctl].revents & (POLLIN | POLLERR | POLLHUP | POLLNVAL))) {
    if (read_child_fdctl(ap, c) < 0)
      return -1;
  }

  if (c->fdout > -1 &&
      (fds[c->fdout].revents & (POLLIN | POLLERR | POLLHUP | POLLNVAL))) {
    if (read_child_stdout(ap, c) < 0)
      return -1;
  }

  if (c->fderr > -1 &&
      (fds[c->fderr].revents & (POLLIN | POLLERR | POLLHUP | POLLNVAL))) {
    if (read_child_stderr(ap, c) < 0)
      return -1;
  }

  return 1;
}

static int read_child_fdctl(alcove_state_t *ap, alcove_child_t *c) {
  unsigned char buf;
  ssize_t n;
  int len = 0;
  char t[MAXMSGLEN] = {0};

  UNUSED(ap);

  n = read(c->fdctl, &buf, sizeof(buf));
  (void)close(c->fdctl);
  c->fdctl = -1;

  if (n == 0) {
    c->fdctl = ALCOVE_CHILD_EXEC;
    len = alcove_mk_atom(t, sizeof(t), "fdctl_closed");

    if (alcove_call_spoof(c->pid, ALCOVE_MSG_CTL, t, len) < 0)
      return -1;
  }

  return 0;
}

static int read_child_stdout(alcove_state_t *ap, alcove_child_t *c) {
  int len = 0;
  char t[MAXMSGLEN] = {0};

  switch (alcove_child_stdio(c->fdout, ap->depth, c, ALCOVE_MSG_TYPE(c))) {
  case 0:
    if (ap->opt & alcove_opt_stdout_closed) {
      len = alcove_mk_atom(t, sizeof(t), "stdout_closed");
      if (alcove_call_spoof(c->pid, ALCOVE_MSG_CTL, t, len) < 0)
        return -1;
    }
    /* fall through */
  case -1:
    (void)close(c->fdout);
    c->fdout = -1;
    break;
  default:
    break;
  }

  return 0;
}

static int read_child_stderr(alcove_state_t *ap, alcove_child_t *c) {
  int len = 0;
  char t[MAXMSGLEN] = {0};

  switch (alcove_child_stdio(c->fderr, ap->depth, c, ALCOVE_MSG_STDERR)) {
  case 0:
    if (ap->opt & alcove_opt_stderr_closed) {
      len = alcove_mk_atom(t, sizeof(t), "stderr_closed");
      if (alcove_call_spoof(c->pid, ALCOVE_MSG_CTL, t, len) < 0)
        return -1;
    }
    /* fall through */
  case -1:
    (void)close(c->fderr);
    c->fderr = -1;
    break;
  default:
    break;
  }

  return 0;
}

static int alcove_handle_signal(alcove_state_t *ap) {
  siginfo_t info = {0};
  int status = 0;
  ssize_t n;

  errno = 0;
  n = read(ALCOVE_SIGREAD_FILENO, &info, sizeof(info));

  if (n != sizeof(info))
    return (errno == EAGAIN || errno == EINTR) ? 0 : -1;

  if (info.si_signo != SIGCHLD || ap->sigchld)
    return alcove_signal_event(ap, &info);

  for (;;) {
    pid_t pid = 0;

    pid = waitpid(-1, &status, WNOHANG);

    if (errno == ECHILD || pid == 0)
      return 0;

    if (pid < 0)
      return -1;

    (void)pid_foreach(ap, pid, &status, NULL, pid_equal, exited_pid);
  }

  return 0;
}

static int alcove_signal_event(alcove_state_t *ap, siginfo_t *info) {
  int index = 0;
  char reply[MAXMSGLEN] = {0};

  UNUSED(ap);

  ALCOVE_TUPLE3(
      reply, sizeof(reply), &index, "signal",
      alcove_signal_name(reply, sizeof(reply), &index, info->si_signo),
      alcove_encode_binary(reply, sizeof(reply), &index, info,
                           (info == NULL ? 0 : sizeof(siginfo_t))));

  if (alcove_call_reply(ALCOVE_MSG_EVENT, reply, index) < 0)
    return -1;

  return 0;
}

static int signal_pid(alcove_state_t *ap, alcove_child_t *c, void *arg1,
                      void *arg2) {
  UNUSED(ap);
  UNUSED(arg1);
  UNUSED(arg2);

  if (c->fdctl == ALCOVE_CHILD_EXEC) {
    if ((kill(-c->pid, c->signaloneof) == 0) ||
        (kill(c->pid, c->signaloneof) == 0)) {
      (void)kill(c->pid, SIGCONT);
    }
  }

  return 1;
}
