-module(boot).

-export([start/0,
         lambda/1,
         call_lambda/0,
         do/1]).

start() ->
    X = random:uniform(),
    Y = X + call_lambda(),
    {ok, Z} = return_tuple(),
    erlang:display(Y + Z).

lambda(Y) ->
    X = random:uniform(),
    fun(Z) ->
            Y + X + Z
    end.

call_lambda() ->
    L = lambda(1),
    L(5).

return_tuple() ->
    {ok, random:uniform()}.

do(X) ->
    random:uniform() + X.
