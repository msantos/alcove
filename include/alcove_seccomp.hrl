% Copyright (c) 2014, Michael Santos <michael.santos@gmail.com>
%
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

%%-------------------------------------------------------------------------
%%% BPF Filter
%%-------------------------------------------------------------------------

%% instruction classes
-define(BPF_CLASS(Code), (Code band 16#07)).
-define(BPF_LD, 16#00).
-define(BPF_LDX, 16#01).
-define(BPF_ST, 16#02).
-define(BPF_STX, 16#03).
-define(BPF_ALU, 16#04).
-define(BPF_JMP, 16#05).
-define(BPF_RET, 16#06).
-define(BPF_MISC, 16#07).

%% ld/ldx fields
-define(BPF_SIZE(Code), (Code band 16#18)).
-define(BPF_W, 16#00).
-define(BPF_H, 16#08).
-define(BPF_B, 16#10).
-define(BPF_MODE(Code), (Code band 16#e0)).
-define(BPF_IMM, 16#00).
-define(BPF_ABS, 16#20).
-define(BPF_IND, 16#40).
-define(BPF_MEM, 16#60).
-define(BPF_LEN, 16#80).
-define(BPF_MSH, 16#a0).

%% alu/jmp fields
-define(BPF_OP(Code), (Code band 16#f0)).
-define(BPF_ADD, 16#00).
-define(BPF_SUB, 16#10).
-define(BPF_MUL, 16#20).
-define(BPF_DIV, 16#30).
-define(BPF_OR, 16#40).
-define(BPF_AND, 16#50).
-define(BPF_LSH, 16#60).
-define(BPF_RSH, 16#70).
-define(BPF_NEG, 16#80).
-define(BPF_JA, 16#00).
-define(BPF_JEQ, 16#10).
-define(BPF_JGT, 16#20).
-define(BPF_JGE, 16#30).
-define(BPF_JSET, 16#40).
-define(BPF_SRC(Code), (Code band 16#08)).
-define(BPF_K, 16#00).
-define(BPF_X, 16#08).

%% ret - BPF_K and BPF_X also apply
-define(BPF_RVAL(Code), (Code band 16#18)).
-define(BPF_A, 16#10).

%% misc
-define(BPF_MISCOP(Code), (Code band 16#f8)).
-define(BPF_TAX, 16#00).
-define(BPF_TXA, 16#80).

%% struct bpf_insn {
%%     u_short code;
%%     u_char  jt;
%%     u_char  jf;
%%     bpf_u_int32 k;
%% };
%% NOTE: the man page says k is a u_long,
%% the header says u_int32_t.
-record(alcove_insn, {
        code = 0 :: 0..65535,
        jt = 0 :: 0..255,
        jf = 0 :: 0..255,
        k = 0 :: non_neg_integer()
    }).

-define(BPF_STMT(Code, K), alcove_seccomp:stmt(Code, K)).
-define(BPF_JUMP(Code, K, JT, JF), alcove_seccomp:jump(Code, K, JT, JF)).


% Valid values for seccomp.mode and prctl(PR_SET_SECCOMP, <mode>)
-define(SECCOMP_MODE_DISABLED, 0).  % seccomp is not in use.
-define(SECCOMP_MODE_STRICT, 1).    % uses hard-coded filter.
-define(SECCOMP_MODE_FILTER, 2).    % uses user-supplied filter.

-define(SECCOMP_RET_KILL, 16#00000000).     % kill the task immediately
-define(SECCOMP_RET_TRAP, 16#00030000).     % disallow and force a SIGSYS
-define(SECCOMP_RET_ERRNO,  16#00050000).   % returns an errno
-define(SECCOMP_RET_TRACE, 16#7ff00000).    % pass to a tracer or disallow
-define(SECCOMP_RET_ALLOW, 16#7fff0000).    % allow

% Masks for the return value sections.
-define(SECCOMP_RET_ACTION, 16#7fff0000).
-define(SECCOMP_RET_DATA, 16#0000ffff).

% struct seccomp_data - the format the BPF program executes over.
% nr: the system call number
% arch: indicates system call convention as an AUDIT_ARCH_* value
%        as defined in <linux/audit.h>.
% instruction_pointer: at the time of the system call.
% args: up to 6 system call arguments always stored as 64-bit values
%        regardless of the architecture.

-record(alcove_seccomp_data, {
        nr = 0 :: integer(),
        arch = 0 :: non_neg_integer(),
        instruction_pointer = 0 :: non_neg_integer(),
        args = [0,0,0,0,0,0] :: [non_neg_integer()]
    }).
