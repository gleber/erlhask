-module(start).

-export([main/0]).

main() ->
    X = simple:do(1),
    erlang:display(X).
