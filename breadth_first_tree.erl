-module(breadth_first_tree).
-moduledoc "Builds a breadth-first tree without centralized control that can then be used for broadcast and convergecast".
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
	    waiting_root_node(Val, Neighbours, ordsets:size(Neighbours));
	{hello, Parent, Level} ->
	    send_hello(Neighbours, Level+1, Parent),
	    undecided_node(Val, Parent, Level+1, Neighbours, [], ordsets:size(Neighbours)-1)
    end.

waiting_root_node(Val, Children, 0) ->
    io:format("Tree formed, finishing up~n"),
    send_finishing(Children),
    finishing_root_node(Val, Children, ordsets:size(Children));
waiting_root_node(Val, Children, WaitingCount) ->
    receive
	{back, Child, yes, Level} ->
	    io:format("Child ~p done~n", [Child]),
	    waiting_root_node(Val, Children, WaitingCount-1)
    end.

finishing_root_node(Val, Children, 0) ->
    io:format("Tree finished, able to collect~n"),
    receive
	collect ->
	    send_collect(Children),
	    collecting_root(Val, Children, [], ordsets:size(Children))
    end;
finishing_root_node(Val, Children, WaitingCount) ->
    receive
	finished ->
	    finishing_root_node(Val, Children, WaitingCount-1)
    end.

collecting_root(Val, Children, Collected, 0) ->
    Computation = lists:sum(Collected) + Val,
    io:format("Finished computation: ~p~n", [Computation]),
    finishing_root_node(Val*2, Children, 0);
collecting_root(Val, Children, Collected, WaitingCount) ->
    receive
	{collected, Partial} ->
	    collecting_root(Val, Children, Partial++Collected, WaitingCount-1)
    end.

undecided_node(Val, Parent, Level, Neighbours, Children, 0) ->
    Parent ! {back, self(), yes, Level},
    decided_node(Val, Parent, Level, Neighbours, Children);
undecided_node(Val, Parent, Level, Neighbours, Children, WaitingCount) ->
    receive
	{hello, NewParent, NewLevel} when NewLevel+1 < Level ->
	    send_hello(Neighbours, NewLevel+1, NewParent),
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
	    send_hello(Neighbours, NewLevel+1, NewParent),
	    undecided_node(Val, NewParent, NewLevel+1, Neighbours, [], ordsets:size(Neighbours)-1);
	{hello, WorseParent, WorseLevel} ->
	    WorseParent ! {back, self(), no, WorseLevel+1};
	finishing ->
	    send_finishing(Children),
	    finishing_node(Val, Parent, Children, ordsets:size(Children))
    end.

finishing_node(Val, Parent, Children, 0) ->
    Parent ! finished,
    settled_node(Val, Parent, Children);
finishing_node(Val, Parent, Children, WaitingCount) ->
    receive
	finished ->
	    finishing_node(Val, Parent, Children, WaitingCount-1)
    end.

settled_node(Val, Parent, Children) ->
    receive
	collect ->
	    send_collect(Children),
	    collecting_node(Val, Parent, Children, [], ordsets:size(Children))
    end.

collecting_node(Val, Parent, Children, Collected, 0) ->
    Parent ! {collected, [Val|Collected]},
    settled_node(Val*2, Parent, Children);
collecting_node(Val, Parent, Children, Collected, WaitingCount) ->
    receive
	{collected, PartialCollected} ->
	    collecting_node(Val, Parent, Children, PartialCollected ++ Collected, WaitingCount-1)
    end.


send_hello(Others, Level, Except) ->
    send_hello(ordsets:del_element(Except, Others), Level).
send_hello(Others, Level) ->
    map(fun (Pid) -> Pid ! {hello, self(), Level} end, Others).
		
send_finishing(Others) ->
    map(fun (Pid) -> Pid ! finishing end, Others).

send_collect(Others) ->
    map(fun (Pid) -> Pid ! collect end, Others).
		
