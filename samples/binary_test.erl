-module(binary_test).

-export([binary_test/0]).

binary_test() ->
    Y = random:uniform(),
    B = <<1,2,3,Y>>,
    L = binary_to_list(B),
    lists:sum(L).
