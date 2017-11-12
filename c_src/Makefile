CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/..)

PROJECT ?= $(notdir $(BASEDIR))
PROJECT := $(strip $(PROJECT))

ERTS_INCLUDE_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~s/erts-~s/include/\", [code:root_dir(), erlang:system_info(version)]).")
ERL_INTERFACE_INCLUDE_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~s\", [code:lib_dir(erl_interface, include)]).")
ERL_INTERFACE_LIB_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~s\", [code:lib_dir(erl_interface, lib)]).")

C_SRC_DIR = $(CURDIR)
C_SRC_OUTPUT ?= $(CURDIR)/../priv/$(PROJECT)

# System type and C compiler/flags.

UNAME_SYS := $(shell uname -s)
ifeq ($(UNAME_SYS), Darwin)
	CC ?= cc
#	CFLAGS ?= -O3 -std=c99 -arch x86_64 -finline-functions -Wall -Wmissing-prototypes
#	LDFLAGS ?= -arch x86_64 -flat_namespace -undefined suppress
else ifeq ($(UNAME_SYS), FreeBSD)
	CC ?= cc
	LDFLAGS ?= -Wno-missing-braces -Wl,-Bsymbolic-functions -Wl,-z,relro,-z,now
	CFLAGS ?= -D_FORTIFY_SOURCE=2 -O2 -fstack-protector-strong -Wformat -Werror=format-security -fno-strict-aliasing
else ifeq ($(UNAME_SYS), OpenBSD)
	CC ?= cc
	LDFLAGS ?= -Wl,-z,relro,-z,now
	CFLAGS ?= -D_FORTIFY_SOURCE=2 -O2 -fstack-protector-strong -Wformat -Werror=format-security -fno-strict-aliasing
else ifeq ($(UNAME_SYS), Linux)
	CC ?= cc
	LDFLAGS ?= -Wl,-Bsymbolic-functions -Wl,-z,relro,-z,now
	CFLAGS ?= -D_FORTIFY_SOURCE=2 -O2 -fstack-protector-strong -Wformat -Werror=format-security -fno-strict-aliasing
else ifeq ($(UNAME_SYS), SunOS)
	LDFLAGS += -lsocket -lnsl
	CFLAGS += -std=c99 -D_POSIX_C_SOURCE=200112L -D__EXTENSIONS__=1 -fno-strict-aliasing
endif

ALCOVE_CFLAGS ?= -g -Wall -fwrapv
CFLAGS += $(ALCOVE_CFLAGS) $(ALCOVE_DEFINE) -I $(C_SRC_DIR) -I $(ERTS_INCLUDE_DIR) -I $(ERL_INTERFACE_INCLUDE_DIR)

LDLIBS += -L $(ERL_INTERFACE_LIB_DIR) $(ALCOVE_LDFLAGS) -lerl_interface -lei

# Verbosity.

c_verbose_0 = @echo " C     " $(?F);
c_verbose = $(c_verbose_$(V))

link_verbose_0 = @echo " LD    " $(@F);
link_verbose = $(link_verbose_$(V))

SOURCES := $(shell find $(C_SRC_DIR) -type f \( -name "*.c" \))
OBJECTS = $(addsuffix .o, $(basename $(SOURCES)))

COMPILE_C = $(c_verbose) $(CC) $(CFLAGS)

$(C_SRC_OUTPUT):
	$(COMPILE_C) -o $(C_SRC_OUTPUT) $(SOURCES) $(LDFLAGS) $(LDLIBS)

clean:
	@rm -f $(C_SRC_OUTPUT) $(OBJECTS) \
		c_src/alcove_call.h \
		c_src/alcove_calls.h \
		c_src/alcove_version.h \
		src/alcove.erl
