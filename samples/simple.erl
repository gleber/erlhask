-module(simple).

-export([main/0,
         lambda/1,
         call_lambda/0,
         do/1]).

main() ->
    X = random:uniform(),
    Y = X + 2,
    erlang:display(Y).

lambda(Y) ->
    X = random:uniform(),
    fun(Z) ->
            Y + X + Z
    end.

call_lambda() ->
    L = lambda(1),
    L(5).

do(X) ->
    random:uniform() + X.
