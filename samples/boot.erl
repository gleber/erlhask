-module(boot).

-export([start/0,
         lambda/1,
         lambdas_test/0,
         call_lambda/0,
         do/1]).

start() ->
    %% try_test().
    catch_test().
    %% apply_test().
    %% process_test().

lambdas_test() ->
    X = random:uniform(),
    Y = X + call_lambda(),
    {ok, Z} = return_tuple(),
    [K, L | _] = return_list(),
    erlang:display(Y + Z + K + L).

lambda(Y) ->
    X = 10,
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

apply_test() ->
    F = fun() ->
                1
        end,
    1 = erlang:apply(F, [1]).

'catch_test'() ->
    F = fun() ->
                throw(aaa)
        end,
    X = (catch erlang:apply(F, [])),
    Y = erlang:get_stacktrace(),
    erlang:display(X).
    

try_test() ->
    try
        X = random:uniform(),
        %% throw(aaa)
        F = fun() ->
                    erlang:error(aaa)
            end,
        erlang:apply(F, [])
        %% erlang:exit(aaa)
    catch
        A:B ->
            erlang:display({A,B,erlang:get_stacktrace()})
    end.

process_test() ->
    Parent = self(),
    Pid = spawn(fun() ->
                        erlang:display("Process spawned!"),
                        Parent ! done
                end),
    receive
        X when erlang:float(X) > 1 -> ok1;
        done -> ok
    end,
    erlang:display({spawned, process, Pid}).
