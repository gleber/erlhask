-module(boot).

-export([start/0,
         lambda/1,
         lambdas_test/0,
         call_lambda/0,
         do/1]).

start() ->
    process_test().

lambdas_test() ->
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
    {ok, random:uniform()}.

return_list() ->
    [1, 2, 3].

do(X) ->
    random:uniform() + X.



process_test() ->
    Pid = spawn(fun() ->
                        erlang:display("Process spawned!")
                end),
    erlang:display({spawned, process, Pid}).
