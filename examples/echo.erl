#!/usr/bin/env escript
%%! -noshell -noinput
-mode(compile).
-define(ME, filename:basename(escript:script_name())).
-define(PRINT(STR, PARAMS), io:format("~s: " ++ STR ++ "~n", [?ME | PARAMS])).

loop(Port) ->
    port_command(Port, term_to_binary({"Hello world!", erlang:now()})),
    receive
        {_, {data, Data}} ->
            ?PRINT("~p", [binary_to_term(Data)]),
            loop(Port)
    end.

main([Bin]) ->
    Port = open_port({spawn, Bin},
                     [binary, {packet, 4}]),
    loop(Port);
main(_) ->
    io:format("~s", ["usage: echo.erl path/to/port"]).