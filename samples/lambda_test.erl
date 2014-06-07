-module(lambda_test).

-export([lambda_test/0]).

lambda_test() ->
    X = 1,
    L = fun(Z) ->
                X + Z
        end,
    L(5).
