#!/usr/bin/env escript

%%%
%%% Generate the alcove.erl file
%%%
main([]) ->
    ModuleName = "alcove",
    Proto = "c_src/alcove_call.proto",
    main([ModuleName, Proto]);
main([ModuleName, Proto]) ->
    Module = erl_syntax:attribute(
        erl_syntax:atom(module),
        [erl_syntax:atom(ModuleName)]
    ),
    Includes = includes(["alcove.hrl"]),

    % Type specs and any hardcoded functions will be included here
    Static = erl_syntax:comment(["%__STATIC__%%"]),

    Calls = calls(Proto),

    % Generate the list of exports
    Comment_gen = erl_syntax:comment([" Generated functions"]),

    Exports_gen1 = erl_syntax:attribute(erl_syntax:atom(export), [
        erl_syntax:list([
            erl_syntax:arity_qualifier(erl_syntax:atom(Fun), erl_syntax:integer(Arity + 2))
         || {Fun, Arity} <- Calls
        ])
    ]),

    Exports_gen2 = erl_syntax:attribute(erl_syntax:atom(export), [
        erl_syntax:list([
            erl_syntax:arity_qualifier(erl_syntax:atom(Fun), erl_syntax:integer(Arity + 3))
         || {Fun, Arity} <- Calls
        ])
    ]),

    % Generate the functions
    Functions = [
        begin
            % name(Drv, ...) -> alcove:call(Drv, [], Fun, [...])
            Arg = arg("Arg", Arity),

            % name(Drv, Pids, ...) -> name(Drv, Pids, ..., infinity)
            Pattern1 = [erl_syntax:variable("Drv"), erl_syntax:variable("Pids") | Arg],
            Body1 = call(Fun, Arg, "infinity"),
            Clause1 = erl_syntax:clause(Pattern1, [], [Body1]),

            % name(Drv, Pids, ..., Timeout) -> alcove:call(Drv, Pids, Fun, [...], Timeout)
            Pattern2 = lists:flatten([
                erl_syntax:variable("Drv"),
                erl_syntax:variable("Pids"),
                Arg,
                erl_syntax:variable("Timeout")
            ]),
            Body2 = call(Fun, Arg, "Timeout"),
            Clause2 = erl_syntax:clause(Pattern2, [], [Body2]),

            Doc = docfile(Fun, Arity),
            [
                Doc,
                erl_syntax:function(erl_syntax:atom(Fun), [Clause1]),
                Doc,
                erl_syntax:function(erl_syntax:atom(Fun), [Clause2])
            ]
        end
     || {Fun, Arity} <- Calls
    ],

    Code0 = erl_prettypr:format(
        erl_syntax:form_list(
            lists:flatten([
                license(),
                Module,
                Includes,

                Comment_gen,
                Exports_gen1,
                Exports_gen2,

                Static,
                Functions
            ])
        )
    ),

    Code = lists:foldl(
        fun({Marker, Generated}, Text) ->
            re:replace(unicode:characters_to_binary(Text), Marker, Generated)
        end,
        Code0,
        [
            {"%%__STATIC__%%", static()}
        ]
    ),

    io:format("~s~n", [Code]).

arg(Prefix, Arity) ->
    [
        erl_syntax:variable(lists:flatten([Prefix, integer_to_list(N)]))
     || N <- lists:seq(1, Arity)
    ].

call(Fun, Arg, Timeout) ->
    erl_syntax:case_expr(
        erl_syntax:application(
            erl_syntax:atom("alcove_drv"),
            erl_syntax:atom("call"),
            [
                erl_syntax:variable("Drv"),
                erl_syntax:variable("Pids"),
                erl_syntax:atom(Fun),
                erl_syntax:list(Arg),
                case Timeout of
                    "infinity" -> erl_syntax:atom("infinity");
                    _ -> erl_syntax:variable(Timeout)
                end
            ]
        ),
        [
            erl_syntax:clause(
                [
                    erl_syntax:tuple([
                        erl_syntax:atom("alcove_error"),
                        erl_syntax:variable("Error")
                    ])
                ],
                none,
                [
                    erl_syntax:application(
                        erl_syntax:atom("erlang"),
                        erl_syntax:atom("error"),
                        [
                            erl_syntax:variable("Error"),
                            erl_syntax:list(
                                lists:flatten([
                                    erl_syntax:variable("Drv"),
                                    erl_syntax:variable("Pids"),
                                    Arg,
                                    case Timeout of
                                        "infinity" -> [];
                                        _ -> erl_syntax:variable("Timeout")
                                    end
                                ])
                            )
                        ]
                    )
                ]
            ),

            erl_syntax:clause(
                [erl_syntax:variable("Reply")],
                none,
                [erl_syntax:variable("Reply")]
            )
        ]
    ).

% List the supported alcove API functions
calls(Proto) ->
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

docfile(Fun, Arity) ->
    File = "edoc/" ++ Fun ++ "-" ++ integer_to_list(Arity + 2) ++ ".md",
    case file:read_file(File) of
        {ok, Bin} ->
            Doc = [
                case X of
                    <<>> -> [];
                    _ -> unicode:characters_to_list([" ", X])
                end
             || X <- re:split(Bin, "\n", [trim])
            ],
            erl_syntax:comment(Doc);
        {error, _} ->
            io:format(standard_error, "docfile missing: ~s~n", [File]),
            halt(111)
    end.

static() ->
    {ok, Hrl} = file:read_file("include/alcove_static.hrl"),
    Hrl.

includes(Header) ->
    [erl_syntax:attribute(erl_syntax:atom(include), [erl_syntax:string(N)]) || N <- Header].

license() ->
    License = [
        " GENERATED: DO NOT EDIT",
        "%% % @noformat"
    ],

    erl_syntax:comment(License).
