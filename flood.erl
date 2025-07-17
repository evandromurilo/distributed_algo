-module(flood).
-moduledoc "Flood algorithm for broadcast and convergecast".
-export([demo/0, waiting_node/1]).
-import(lists, [map/2, sum/1, all/2, member/2]).

demo() ->
    A = spawn(flood, waiting_node, [5]),
    B = spawn(flood, waiting_node, [2]),
    C = spawn(flood, waiting_node, [6]),
    D = spawn(flood, waiting_node, [1]),
    E = spawn(flood, waiting_node, [12]),
    F = spawn(flood, waiting_node, [28]),
    G = spawn(flood, waiting_node, [1]),
    H = spawn(flood, waiting_node, [5]),
    A ! {neighbours, A, [B, C]}, % root node of the tree
    B ! {neighbours, A, [D]},
    C ! {neighbours, A, [E, F]},
    D ! {neighbours, B, [G]},
    E ! {neighbours, C, []},
    F ! {neighbours, C, []},
    G ! {neighbours, D, [H]},
    H ! {neighbours, G, []},
    A ! {A, broadcast}, % each time you send, it will double
    A.

waiting_node(Value) ->
    receive
	{neighbours, Parent, Children} ->
	    loop(Parent, Children, Value, [])
    end.

loop(Parent, [], Value, ValueSet) -> % leaf
    receive
	{Pid, broadcast} ->
	    io:format("Leaf ~p~n", [Value]),
	    Parent ! {Pid, value, [{self(), Value}]},
	    loop(Parent, [], Value*2, ValueSet)
    end;
loop(Parent, Children, Value, ValueSet) ->
    receive
	{Pid, broadcast} ->
	    map(fun (Child) -> Child ! {Pid, broadcast} end, Children),
	    loop(Parent, Children, Value, ValueSet);
	{Pid, value, Set} ->
	    NewSet = ordsets:union(ValueSet, Set),
	    io:format("NewSet ~p~n", [NewSet]),
	    AlreadyHere = [X || {X, _} <- NewSet],
	    case all(fun (Child) -> member(Child, AlreadyHere) end, Children) of
		true ->
		    case Pid =:= self() of
			true -> % all collected at destination of convergecast
			    Computation = Value + sum([X || {_, X} <- NewSet]),
			    io:format("Computed value: ~p~n", [Computation]),
			    loop(Parent, Children, Value*2, []);
			false -> % all collected at middle branch
			    Parent ! {Pid, value, ordsets:union(NewSet, [{self(), Value}])},
			    loop(Parent, Children, Value*2, [])
		    end;
		false -> % not all collected
		    loop(Parent, Children, Value, NewSet)
	    end
    end.
   
		    
		    
	    
	    
		

			
	    
	    
