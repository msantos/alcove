#ifndef HAVE_SETPROCTITLE
void spt_init(int argc, char *argv[]);
void setproctitle(const char *fmt, ...);
#endif
