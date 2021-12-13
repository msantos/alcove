#!/usr/bin/env escript

%%%
%%% Generate the alcove version from the app file
%%%
main([]) ->
    File = "src/alcove.app.src",
    main([File]);
main([File]) ->
    {ok, [{application, alcove, App}]} = file:consult(File),
    VSN = proplists:get_value(vsn, App),
    io:format(
        "/* GENERATED: DO NOT EDIT */~n#define ALCOVE_VERSION \"~s\"~n",
        [VSN]
    ).
