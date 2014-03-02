#!/usr/bin/env escript

%%%
%%% Generate the alcove version from the app file
%%%
main([]) ->
    Header = "c_src/alcove_version.h",
    main([Header]);

main([Header]) ->
    {ok, [{application, alcove, App}]} = file:consult("src/alcove.app.src"),
    VSN = proplists:get_value(vsn, App),
    file:write_file(Header, "#define ALCOVE_VERSION \"" ++ VSN ++ "\"\n").
