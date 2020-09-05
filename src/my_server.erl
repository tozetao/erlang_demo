-module(my_server).

%% API
-export([start/0, add/2, get/1, info/0]).
-export([init/0, handle_call/2, handle_cast/2]).
-define(Server, ?MODULE).

-record(state, {map}).

%% 实现一个简单的server
start() ->
    register(?Server, spawn(fun() -> loop(?Server:init(), ?Server) end)).

init() ->
    #state{map = dict:new()}.

add(Key, Val) ->
    cast({add, {Key, Val}}).

get(Key) ->
    {ok, Res} = call({get, Key}),
    Res.

info() ->
    {ok, Res} = call(info),
    Res.

handle_call({get, Key}, #state{map = State}) ->
    {ok, Value} = dict:find(Key, State),
    {reply, Value};

handle_call(info, #state{map = State}) ->
    {reply, dict:to_list(State)}.


handle_cast({add, {Key, Val}}, State = #state{map = Map}) ->
    NewMap = dict:store(Key, Val, Map),
    {noreply, State#state{map = NewMap}}.



call(Data) ->
    ?Server ! {sync, {self(), Data}},

    receive
        {reply, Res} ->
            {ok, Res};
        {reply, Res, State} ->
            {ok, Res, State};
        _ ->
            error
    after 5 * 1000 ->
        timeout
    end.

cast(Data) ->
    ?Server ! {async, {self(), Data}},
    ok.

loop(State, Module) ->
    receive
        %% 同步处理
        {sync, {From, Data}} ->
            case Module:handle_call(Data, State) of
                {reply, Res} ->
                    From ! {reply, Res},
                    loop(State, Module);
                {reply, Res, NewState} ->
                     From ! {reply, Res, NewState},
                     loop(NewState, Module);
                Reason ->
                    From ! {error, Reason},
                    loop(State, Module)
            end;

        %% 异步处理
        {async, {_From, Data}} ->
            case Module:handle_cast(Data, State) of
                {noreply, NewState}  ->
                    loop(NewState, Module);
                _ ->
                    loop(State, Module)
            end
    end.

log(Param) ->
    io:format("~p\n", [Param]).