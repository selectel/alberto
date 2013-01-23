```
         __
        /  \
       / ..|\
      (_\  |_)
      /  \@'    Alberto!
     /     \         0.1
_  _/  `   |
\\/  \  | _\
 \   /_ || \\_
  \____)|_) \_)
```

Alberto is a simple OCaml library for writing Erlang port drivers.

Installation
------------

The simplest way to get `alberto` is to use [`OPAM`](opam.ocamlpro.com):

```bash
opam install alberto
```

Example
-------

On Erlang part we have a simple echo server, which sends `<<"Hello world!">>`
to the port driver and echoes whatever comes back to stdout:

```erlang
#!/usr/bin/env escript
%%! -noshell -noinput
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
```

OCaml part is even more consice:

```ocaml
Alberto.interact (fun x -> x)
```

Okay, let's see it in action (sources are available in `examples/` directory):

```bash
$ make
$ examples/echo.erl ./port_simple.native
echo.erl: "Hello world!"
echo.erl: "Hello world!"
echo.erl: "Hello world!"
echo.erl: "Hello world!"
```

**Note**: to build examples in the `examples/` directory configure
`alberto` with `--enable-examples` flag.
