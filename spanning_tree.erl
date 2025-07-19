-module(spanning_tree).
-export([demo/0, waiting_node/1]).

demo() ->
    A = spawn(spanning_tree, waiting_node, [5]),
    B = spawn(spanning_tree, waiting_node, [2]),
    C = spawn(spanning_tree, waiting_node, [6]),
    D = spawn(spanning_tree, waiting_node, [1]),
    E = spawn(spanning_tree, waiting_node, [12]),
    F = spawn(spanning_tree, waiting_node, [28]),
    G = spawn(spanning_tree, waiting_node, [1]),
    H = spawn(spanning_tree, waiting_node, [5]),
    A ! {neighbours, [B, C, G]},
    B ! {neighbours, [D, H, E]},
    C ! {neighbours, [E, F]},
    D ! {neighbours, [G]},
    E ! {neighbours, [F, B]},
    F ! {neighbours, [E]},
    G ! {neighbours, [H, A]},
    H ! {neighbours, [B]},
    A ! start, % could be any node
    A.

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
	    done;
	_ ->
	    Parent ! {self(), back, NewValSet}
    end,
    done; % todo wait for new messages?
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
		
