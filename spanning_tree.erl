-module(spanning_tree).
-moduledoc "Builds a spanning tree rooted at 'Pa' that can be reused for broadcast and convergecast".
-export([demo/0, waiting_node/1]).

demo() ->
    A = spawn(spanning_tree, waiting_node, [2]),
    B = spawn(spanning_tree, waiting_node, [4]),
    C = spawn(spanning_tree, waiting_node, [12]),
    D = spawn(spanning_tree, waiting_node, [1]),
    E = spawn(spanning_tree, waiting_node, [7]),
    F = spawn(spanning_tree, waiting_node, [8]),
    G = spawn(spanning_tree, waiting_node, [9]),
    H = spawn(spanning_tree, waiting_node, [21]),
    A ! {neighbours, [B, C, G]},
    B ! {neighbours, [A, H]},
    C ! {neighbours, [A, E, F]},
    D ! {neighbours, [G]},
    E ! {neighbours, [F, C]},
    F ! {neighbours, [E, C]},
    G ! {neighbours, [H, A, D]},
    H ! {neighbours, [B]},
    D ! start, % could be any node
    D.

waiting_node(Value) ->
    receive
	{neighbours, Neighbours} ->
	    floating_node(Value, Neighbours)
    end.

floating_node(Value, Neighbours) ->
    receive
	start ->
	    io:format("Root node is ~p~n", [self()]),
	    send_hello(self(), Neighbours),
	    loop(Value, self(), Neighbours, [], []); 
	{Parent, hello} ->
	    io:format("~p is parent of ~p~n", [Parent, self()]),
	    send_hello(self(), Neighbours),
	    loop(Value, Parent, Neighbours, [], [])
    end.

loop(Value, Parent, [], Children, ValSet) ->
    NewValSet = ordsets:add_element({self(), Value}, ValSet),
    io:format("~p Done! Collected: ~p~n", [self(), NewValSet]),
    case self() of
	Parent ->
	    Computation = lists:sum([X || {_, X} <- NewValSet]),
	    io:format("~p computed: ~p~n", [self(), Computation]);
	_ ->
	    Parent ! {self(), back, NewValSet}
    end,
    floating_node(Value*2, Children);
loop(Value, Parent, Waiting, Children, ValSet) ->
    receive
	{Late, hello} ->
	    Late ! {self(), back, []},
	    loop(Value, Parent, Waiting, Children, ValSet);
	{Late, back, []} ->
	    loop(Value, Parent, lists:delete(Late, Waiting), Children, ValSet);
	{Child, back, ChildValSet} ->
	    loop(Value, Parent, lists:delete(Child, Waiting), ordsets:add_element(Child, Children), ordsets:union(ChildValSet, ValSet))
    end.

send_hello(Me, Neighbours) ->
    lists:map(fun (Pid) -> Pid ! {Me, hello} end, Neighbours).
