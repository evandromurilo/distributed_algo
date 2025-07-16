-module(learn_graph).
-moduledoc "Um algoritmo para que nodes conheçam o grafo inteiro".
-export([demo/0, unitialized_node/0]).
-import(lists, [map/2, member/2, all/2]).

demo() ->
    A = spawn(learn_graph, unitialized_node, []),
    B = spawn(learn_graph, unitialized_node, []),
    C = spawn(learn_graph, unitialized_node, []),
    D = spawn(learn_graph, unitialized_node, []),
    E = spawn(learn_graph, unitialized_node, []),
    A ! {neighbours, [B, C]},
    B ! {neighbours, [A, D]},
    C ! {neighbours, [A]},
    D ! {neighbours, [B, E]},
    E ! {neighbours, [B]},
    A ! start.

% primeiro a node espera para conhecer sua posição (seus vizinhos)
unitialized_node() ->
    receive
	{neighbours, Neighbours} ->
	    KChan = chan_set(self(), Neighbours),
	    nonstarted_node(Neighbours, [self()], KChan)
    end.

nonstarted_node(Neighbours, KProc, KChan) ->
    receive
	start ->
	    make_first_contact(Neighbours);
	Pos ->
	    make_first_contact(Neighbours),
	    self() ! Pos
    end,
    loop(Neighbours, KProc, KChan).

	
make_first_contact(Neighbours) ->
    io:format("First contact of ~p with ~p!~n", [self(), Neighbours]),
    map(fun(Who) -> Who ! {self(), position, Neighbours} end, Neighbours).
    
loop(Neighbours, KProc, KChan) ->
    io:format("~p is knowing ~p Kproc and ~p Kchan~n", [self(), KProc, KChan]),
    Done = all(fun({Na, Nb}) -> member(Na, KProc) and member(Nb, KProc) end, KChan),
    if Done ->
	    io:format("Done!~n"),
	    finished_node();
       true ->
	    ok % keep going
    end,

    receive
	{Who, position, HisNeighbours} ->
	    io:format("~p received pos from ~p!~n", [self(), Who]),
	    
	    Member = member(Who, KProc),
	    if not Member ->
		    map(fun % todo itself
			   (Other) -> Other ! {Who, position, HisNeighbours} end, Neighbours), % repassa o conhecimento aos vizinhos
		    loop(Neighbours, [Who|KProc], ordsets:union(KChan, chan_set(Who, HisNeighbours)));
	       true -> ok % keep going
	    end,
	    loop(Neighbours, KProc, KChan)
    end.

finished_node() ->
    finished_node(). % how to stop?

chan_set(S, L) ->
    ordsets:from_list([chan(S, X) || X <- L]).

-doc "Representação canônica de um canal (apenas o par Pa, Pb ordenado)".
chan(Pa, Pb) ->
    {min(Pa, Pb), max(Pa, Pb)}.
