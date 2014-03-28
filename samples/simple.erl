-module(simple).

-export([main/0]).

main() ->
    X = random:uniform(),
    Y = X + 2,
    erlang:display(Y).
