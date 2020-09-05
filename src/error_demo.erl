-module(error_demo).

%% API
-export([start/0, test/0, test2/0]).

test() ->
    io:format("hello world...").

test2() ->
    io:format("hello test2").

start() ->
    io:format("hi\n"),
    spawn(fun() -> loop() end).

loop() ->
    receive
        a ->
            io:format("a\n"),
            loop();
        {cost, K} ->
            io:format("cost: ~p\n", [cost(K)]),
            loop()
    end.

cost(oranges) -> 1;
cost(apples) -> 2;
cost(milk) -> 3.