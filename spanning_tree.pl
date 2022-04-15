:- dynamic edge/2, edge_curr/2, node/1.

/* Read line from stdin */
read_line(L,C) :-
	get_char(C),
	(isEOFEOL(C), L = [], !;
		read_line(LL,_),
		[C|LL] = L).


/* Check if EOF or EOL is matched */
isEOFEOL(C) :-
	C == end_of_file;
	(char_code(C,Code), Code==10).


/* Load lines from the stdin */
read_lines(Ls) :-
	read_line(L,C),
	( C == end_of_file, Ls = [] ;
	  read_lines(LLs), Ls = [L|LLs]
	).

/* Print edge to STDOUT */
write_edge([A,B]):- format('~w-~w', [A, B]).

/* Print spanning tree to STDOUT */
write_line([]):- nl.
write_line([H|[]]):- write_edge(H), write_line([]).
write_line([H|T]):- write_edge(H), write(' '), write_line(T).

/* Print all possible spanning trees */
write_lines2([]).
write_lines2([H|T]) :- write_line(H), write_lines2(T).

main :-
    prompt(_, ''),
    read_lines(LL), /* load input and save it to dynamic predicates */
    load_edges(LL),
    load_nodes,
	findall(Tree, (get_tree(Tree), is_spanning_tree(Tree)), Trees), /* find all spanning trees */
    write_lines2(Trees),
    halt.

is_spanning_tree(Tree):- 
	foreach(member([X, Y], Tree),assertz(edge_curr(X,Y))),
	node(Node), !, 
	(has_cycle(Node, -1, []) -> 
		(retractall(edge_curr(X,Y)), false); 
		(visit_all(Node, [], []) ->
			retractall(edge_curr(X,Y));
			(retractall(edge_curr(X,Y)), false))).

subset_l([], []).
subset_l([E|Tail], [E|NTail]):-
	subset_l(Tail, NTail).
subset_l([_|Tail], NTail):-
	subset_l(Tail, NTail).

/* Check whatever tree has cycle */
has_cycle(Curr, Prev, Visited):-
	subtract(Visited, [Prev], Vis),
	member(Curr, Vis) , !. 
has_cycle(Curr, Prev, Visited):-
	(edge_curr(Curr, Next); edge_curr(Next, Curr)),
	Next \= Prev,
	has_cycle(Next, Curr, [Curr|Visited]).



visit_all(Node, Visited, Result):-
	get_neighbors(Node,Neighbors), 
	subtract(Neighbors, Visited, Can_visit), 
	member(Next, Can_visit), 
	union(Result, Neighbors, Reachable), 
	visit_all(Next, [Node|Visited], [Node|Reachable]).	
visit_all(Node, Visited,Result):-
	get_nodes(Nodes), 
	subtract(Nodes, Result, Diff), 
	is_empty(Diff).

/* Check whatever list is empty */
is_empty(List):- not(member(_,List)).


/* Get all node neighors */
get_neighbors(Node, Neighbors):- 
	findall(Y, edge_curr(Node,Y), Left), 
	findall(Y, edge_curr(Y, Node), Right), 
	union(Left, Right, Neighbors).

/* Get single tree */
get_tree(X):- findall([X,Y], edge(X,Y), Ls), subset_l(Ls, X).

/* Get all nodes */
get_nodes(Nodes):- setof(X, node(X), Nodes).

/* Save all nodes to the database */
load_nodes:- 
	setof(X, Y^edge(X,Y), Ls), 
	setof(X, Y^edge(Y,X), Rs), 
	union(Ls, Rs, Nodes), 
	foreach(member(X, Nodes),assertz(node(X))).

/* Preprocess each line to the Edge */
load_edge([X |[' ', Y]]):- X \= Y, not(edge(X,Y)),  assertz(edge(X,Y)).

/* Load graph edges */
load_edges([]).
load_edges([L|Ls]) :- load_edges(Ls), load_edge(L).

