-module(breadth_first_tree).
-export([demo/0, waiting_node/1]).
-import(lists, [map/2]).

demo() ->
    A = spawn(breadth_first_tree, waiting_node, [2]),
    B = spawn(breadth_first_tree, waiting_node, [4]),
    C = spawn(breadth_first_tree, waiting_node, [12]),
    D = spawn(breadth_first_tree, waiting_node, [1]),
    E = spawn(breadth_first_tree, waiting_node, [7]),
    F = spawn(breadth_first_tree, waiting_node, [8]),
    G = spawn(breadth_first_tree, waiting_node, [9]),
    H = spawn(breadth_first_tree, waiting_node, [21]),
    erlang:trace(A, true, [send, 'receive']),
    A ! {neighbours, [B, C, G]},
    B ! {neighbours, [A, H]},
    C ! {neighbours, [A, E, F]},
    D ! {neighbours, [G]},
    E ! {neighbours, [F, C]},
    F ! {neighbours, [E, C]},
    G ! {neighbours, [H, A, D]},
    H ! {neighbours, [B, G]},
    D ! start, % could be any node
    D.

waiting_node(Val) ->
    receive
	{neighbours, List} ->
	    orphan_node(Val, ordsets:from_list(List))
    end.

orphan_node(Val, Neighbours) ->
    receive
	start ->
	    send_hello(Neighbours, 0),
	    root_node(Val, Neighbours, ordsets:size(Neighbours));
	{hello, Parent, Level} ->
	    send_hello(Neighbours, Level+1, Parent),
	    undecided_node(Val, Parent, Level+1, Neighbours, [], ordsets:size(Neighbours)-1)
    end.

root_node(Val, Children, 0) ->
    io:format("Done");
root_node(Val, Children, WaitingCount) ->
    receive
	{back, Child, yes, Level} ->
	    io:format("Child ~p done~n", [Child]),
	    root_node(Val, Children, WaitingCount-1)
    end.

undecided_node(Val, Parent, Level, Neighbours, Children, 0) ->
    Parent ! {back, self(), yes, Level},
    decided_node(Val, Parent, Level, Neighbours, Children);
undecided_node(Val, Parent, Level, Neighbours, Children, WaitingCount) ->
    receive
	{hello, NewParent, NewLevel} when NewLevel+1 < Level ->
	    send_hello(Neighbours, NewLevel+1),
	    undecided_node(Val, NewParent, NewLevel+1, Neighbours, [], ordsets:size(Neighbours)-1);
	{hello, WorseParent, WorseLevel} ->
	    WorseParent ! {back, self(), no, WorseLevel+1},
	    undecided_node(Val, Parent, Level, Neighbours, Children, WaitingCount);
	{back, Child, yes, CLevel} when CLevel =:= Level+1  ->
	    undecided_node(Val, Parent, Level, Neighbours, ordsets:add_element(Child, Children), WaitingCount-1);
	{back, _, no, CLevel} when CLevel =:= Level+1->
	    undecided_node(Val, Parent, Level, Neighbours, Children, WaitingCount-1)
    end.

decided_node(Val, Parent, Level, Neighbours, Children) ->
    receive
	{hello, NewParent, NewLevel} when NewLevel+1 < Level ->
	    send_hello(Neighbours, NewLevel+1),
	    undecided_node(Val, NewParent, NewLevel+1, Neighbours, [], ordsets:size(Neighbours)-1);
	{hello, WorseParent, WorseLevel} ->
	    WorseParent ! {back, self(), no, WorseLevel+1}
    end.

send_hello(Others, Level, Except) ->
    send_hello(ordsets:del_element(Except, Others), Level).
send_hello(Others, Level) ->
    map(fun (Pid) -> Pid ! {hello, self(), Level} end, Others).
		
