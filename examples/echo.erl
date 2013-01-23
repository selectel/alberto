#!/usr/bin/env escript
%%! -noshell -noinput
%% -*- mode: erlang -*-
-mode(compile).
-define(ME, filename:basename(escript:script_name())).
-define(PRINT(STR, PARAMS), io:format("~s: " ++ STR ++ "~n", [?ME | PARAMS])).


main([Bin]) ->
    Port = open_port({spawn, Bin},
                     [binary, {packet, 4}]),
    port_command(Port, term_to_binary("Hello world!")),
    receive
        {_, {data, Data}} ->
            ?PRINT("~p", [binary_to_term(Data)]),
            main([Bin])
    end;
main(_) ->
    io:format("~s", ["usage: echo.erl path/to/port"]).

