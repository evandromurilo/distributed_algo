-module(learn_graph).
-moduledoc "Um algoritmo para que nodes conheçam o grafo inteiro".
-export([demo/0, unitialized_node/0]).
-import(lists, [map/2, member/2, all/2]).

demo() ->
    A = spawn(learn_graph, unitialized_node, []),
    B = spawn(learn_graph, unitialized_node, []),
    C = spawn(learn_graph, unitialized_node, []),
    D = spawn(learn_graph, unitialized_node, []),
    A ! {neighbours, [B, C]},
    B ! {neighbours, [A, D]},
    C ! {neighbours, [A]},
    D ! {neighbours, [B]},
    A ! start.

% primeiro a node espera para conhecer sua posição (seus vizinhos)
unitialized_node() ->
    receive
	{neighbours, Neighbours} ->
	    KChan = chan_set(self(), Neighbours),
	    initialized_node(false, Neighbours, [self()], KChan)
    end.

make_first_contact(Neighbours) ->
    io:format("First contact of ~p with ~p!~n", [self(), Neighbours]),
    map(fun(Who) -> Who ! {self(), position, Neighbours} end, Neighbours).
    
initialized_node(Started, Neighbours, KProc, KChan) ->
    io:format("~p is knowing ~p Kproc and ~p Kchan~n", [self(), KProc, KChan]),
    Done = all(fun({Na, Nb}) -> member(Na, KProc) and member(Nb, KProc) end, KChan),
    if Done ->
	    io:format("Done!~n"),
	    finished_node();
       true ->
	    ok % keep going
    end,

    receive
	start ->
	    if not Started ->
		    make_first_contact(Neighbours);
	       true ->
		    ok % keep going
	    end, % se já tinha mandado a primeira comunicação, apenas consumimos a msg start
	    initialized_node(true, Neighbours, KProc, KChan);
	{Who, position, HisNeighbours} ->
	    io:format("~p received pos from ~p!~n", [self(), Who]),
	    if not Started ->
		    make_first_contact(Neighbours);
	       true -> 
		    ok % keep going
	    end,

	    Member = member(Who, KProc),
	    if not Member ->
		    map(fun % todo itself
			   (Other) -> Other ! {Who, position, HisNeighbours} end, Neighbours), % repassa o conhecimento aos vizinhos
		    initialized_node(true, Neighbours, [Who|KProc], ordsets:union(KChan, chan_set(Who, HisNeighbours)));
	       true -> ok % keep going
	    end,
	    initialized_node(true, Neighbours, KProc, KChan)
    end.

finished_node() ->
    finished_node(). % how to stop?

s_zip(S, L) ->
    [{S, X} || X <- L].

chan_set(S, L) ->
    ordsets:from_list(map(fun ({Pa, Pb}) -> chan(Pa, Pb) end, s_zip(S, L))).
		

-doc "Representação canônica de um canal (apenas o par Pa, Pb ordenado)".
chan(Pa, Pb) ->
    {min(Pa, Pb), max(Pa, Pb)}.
