-module(boot).

-export([start/0,
         lambda/1,
         call_lambda/0,
         do/1]).

start() ->
    X = random:uniform(),
    Y = X + call_lambda(),
    {ok, Z} = return_tuple(),
    [K, L | _] = return_list(),
    erlang:display(Y + Z + K + L).

lambda(Y) ->
    X = random:uniform(),
    fun(Z) ->
            Y + X + Z
    end.

call_lambda() ->
    L = lambda(1),
    L(5).

return_tuple() ->
    {ok, random:uniform(), 1}.

return_list() ->
    [1, 2, 3].

do(X) ->
    random:uniform() + X.
