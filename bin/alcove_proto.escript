#!/usr/bin/env escript

%%%
%%% Generate the alcove.erl file
%%%
%%% % @noformat
main([]) ->
    ModuleName = "alcove_proto",
    Proto = "c_src/alcove_call.proto",
    main([ModuleName, Proto]);
main([ModuleName, Proto]) ->
    Module = erl_syntax:attribute(
        erl_syntax:atom(module),
        [erl_syntax:atom(ModuleName)]
    ),
    Includes = includes(["alcove.hrl"]),

    % Type specs
    Specs = erl_syntax:comment(["%__SPECS__%%"]),

    % Any hardcoded functions will be included here
    Static = erl_syntax:comment(["%__STATIC__%%"]),

    % Generate the list of exports
    Comment_static = erl_syntax:comment([" Static functions"]),
    Exports_static = erl_syntax:attribute(erl_syntax:atom(export), [
        erl_syntax:list([
            erl_syntax:arity_qualifier(erl_syntax:atom(Fun), erl_syntax:integer(Arity))
         || {Fun, Arity} <- static_exports()
        ])
    ]),

    Comment_gen = erl_syntax:comment([" Generated functions"]),
    Exports_gen = erl_syntax:attribute(erl_syntax:atom(export), [
        erl_syntax:list([
            erl_syntax:arity_qualifier(erl_syntax:atom(calls), erl_syntax:integer(0))
        ])
    ]),

    Calls = proto(Proto),

    Body = erl_syntax:list([erl_syntax:atom(N) || {N, _} <- Calls]),
    Clause = erl_syntax:clause([], [], [Body]),
    Fun = [erl_syntax:function(erl_syntax:atom("calls"), [Clause])],

    Code0 = erl_prettypr:format(
        erl_syntax:form_list(
            lists:flatten([
                license(),
                Module,
                Includes,

                Specs,

                Comment_static,
                Exports_static,

                Comment_gen,
                Exports_gen,

                Static,
                Fun
            ])
        )
    ),

    Code = lists:foldl(
        fun({Marker, Generated}, Text) ->
            re:replace(Text, Marker, Generated)
        end,
        Code0,
        [
            {"%%__STATIC__%%", static()},
            {"%%__SPECS__%%", specs(Calls)}
        ]
    ),

    io:format("~s~n", [Code]).

% List the supported alcove API functions
proto(Proto) ->
    {ok, Bin} = file:read_file(Proto),
    Fun = binary:split(Bin, <<"\n">>, [trim, global]),
    call_to_fun(Fun, []).

call_to_fun([], Acc) ->
    lists:reverse(Acc);
call_to_fun([H | T], Acc) ->
    [Fun, Arity] = binary:split(H, <<"/">>),
    call_to_fun(T, [{binary_to_list(Fun), b2i(Arity)} | Acc]).

b2i(N) when is_binary(N) ->
    list_to_integer(binary_to_list(N)).

static_exports() ->
    [{call, 1}, {noreturn, 1}].

static() ->
    [static({Fun, Arity}) || {Fun, Arity} <- static_exports()].

static({call,1}) ->
"
call(Call) when is_atom(Call) ->
    lookup(Call, calls(), 0).

lookup(Call, [Call|_], N) ->
    N;
lookup(Call, [_|Calls], N) ->
    lookup(Call, Calls, N+1).
";

static({noreturn,1}) ->
"
noreturn(execve) -> true;
noreturn(execvp) -> true;
noreturn(exit) -> true;
noreturn(fexecve) -> true;
noreturn(_) -> false.
".

includes(Header) ->
    [erl_syntax:attribute(erl_syntax:atom(include), [erl_syntax:string(N)]) || N <- Header].

% FIXME hack for hard coding typespecs
specs([{Call, _} | Calls] = X) ->
    Max = integer_to_list(length(X)),
    lists:flatten([
        "-export_type([call/0, calls/0]).",
        "\n"
        "\n",
        "-type call() :: ",
        Call,
        [[" | ", N] || {N, _} <- Calls],
        "."
        "\n"
        "\n",
        "-spec call(call()) -> 0..",
        Max,
        ".",
        "\n",
        "-spec noreturn(call()) -> boolean().",
        "\n",
        "-type calls() :: [call()].",
        "\n",
        "-spec calls() -> [call(),...]."
    ]).

license() ->
    License = [
        " GENERATED: DO NOT EDIT",
        "%% % @noformat"
    ],

    erl_syntax:comment(License).
