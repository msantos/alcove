%% Copyright (c) 2014, Michael Santos <michael.santos@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
-module(alcove_ioctl).

-export([
        in/0, out/0, void/0,
        in/1, out/1, void/1,
        ioc/4,
        io/2, ior/3,
        iow/3, iowr/3
    ]).


%%
%% Calculate the values of the ioctl request macros at runtime.
%%

%% On BSD systems, the top 3 bits indicate the direction of the request:
%%
%%     8 : ioctl parameter will be copied from user space into the kernel
%%     4 : ioctl parameter will be copied into the user supplied buffer from
%%         the kernel
%%     2 : no parameters are used
%%
%% The remaining 13 bits are used to indicate the size of the parameter.
%%
%% On Linux systems, only the top 2 bits are used. Setting both bits to 0
%% indicates a void parameter. The meaning of the bits is reversed:
%%
%%     8 : ioctl parameter will be copied into the user supplied buffer from
%%         the kernel
%%     4 : ioctl parameter will be copied from user space into the kernel
%%
%% The remaining 14 bits is used to hold the length of the parameter.
%%
%% BSD: <<IN:1, OUT:1, VOID:1, Length:13, Group:8, Command:8>>
%%
%% _IOC_OUT = 0x40000000
%% _IOC_IN  = 0x80000000
%%
%% #define _IOR(g,n,t) _IOC(IOC_OUT, (g), (n), sizeof(t))
%% #define _IOW(g,n,t) _IOC(IOC_IN, (g), (n), sizeof(t))
%%
%% Linux: <<OUT:1, IN:1, Length:14, Group:8, Command:8>>
%%
%% _IOC_OUT = 0x80000000
%% _IOC_IN  = 0x40000000
%%
%% #define _IOR(g,n,t) _IOC(_IOC_READ,(g),(n),(t))
%% #define _IOW(g,n,t) _IOC(_IOC_WRITE,(g),(n),(t))
%%

%% BSD: _IOC_IN
%% Linux: _IOC_WRITE << _IOC_DIRSHIFT (16)
in() -> in(os()).
in(bsd) -> 16#80000000;
in(linux) -> 16#40000000.

%% BSD: _IOC_OUT
%% Linux: _IOC_READ << _IOC_DIRSHIFT (16)
out() -> out(os()).
out(bsd) -> 16#40000000;
out(linux) -> 16#80000000.

%% BSD: _IOC_VOID
%% Linux: _IOC_NONE
void() -> void(os()).
void(bsd) -> 16#20000000;
void(linux) -> 0.

-define(IOCPARM_MASK, 16#1fff).

ioc(Inout,Group,Num,Len) ->
    Size = case os() of
        bsd -> Len band ?IOCPARM_MASK;
        linux -> Len
    end,
    Inout bor (Size bsl 16) bor (Group bsl 8) bor Num.

io(Type,NR) ->
    ioc(void(), Type, NR, 0).

ior(Type,NR,Size) ->
    ioc(out(), Type, NR, Size).

iow(Type,NR,Size) ->
    ioc(in(), Type, NR, Size).

iowr(Type,NR,Size) ->
    ioc(in() bor out(), Type, NR, Size).


os() ->
    case os:type() of
        {unix, linux} -> linux;
        {unix,BSD} when BSD == darwin;
            BSD == openbsd;
            BSD == netbsd;
            BSD == freebsd -> bsd
    end.
