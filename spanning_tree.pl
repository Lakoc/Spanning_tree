:- dynamic edge/2, edge_curr/2, node/1.

main :-
    prompt(_, ''),
    read_lines(LL), /* load input and save it to dynamic predicates */
    load_edges(LL),
    load_nodes,
	findall(Tree, (get_tree(Tree), is_spanning_tree(Tree)), Trees), /* find all spanning trees */
    write_lines2(Trees),
    halt.

/* Check whatever input graph is spanning tree */
is_spanning_tree(Tree):- 
	foreach(member([X, Y], Tree),assertz(edge_curr(X,Y))), /* insert current node to dynamic "database"*/
	node(Node), !, /* select random node from that tree is explored */
	(has_cycle(Node, -1, []) -> 
		(retractall(edge_curr(X,Y)), false);  /* cycle was found, clear db, and return with false */
		(visit_all(Node, [], []) -> 
			retractall(edge_curr(X,Y)); /* spanning tree was found */
			(retractall(edge_curr(X,Y)), false))). /* not every node is accesible, return false */

/* Check whatever graph has cycle */
has_cycle(Curr, Prev, Visited):-
	subtract(Visited, [Prev], Vis), /* do not enable back-loop to prev node */
	member(Curr, Vis) , !. /* node that was explored before was found -> graph has cycle */

/* No cycle found at the time, explore further */
has_cycle(Curr, Prev, Visited):-
	(edge_curr(Curr, Next); edge_curr(Next, Curr)), /* get next node */
	Next \= Prev, /* disable prev node */
	has_cycle(Next, Curr, [Curr|Visited]). /* recursively call self with enlarged visited list */

/* Recursively explore graph, by gradually enlarge Reachable list */
visit_all(Node, Visited, Reachable):-
	get_neighbors(Node,Neighbors), 
	subtract(Neighbors, Visited, Can_visit), 
	member(Next, Can_visit), /* get next node to visit */
	union(Reachable, Neighbors, Reachable_New), 
	visit_all(Next, [Node|Visited], [Node|Reachable_New]). /* add also current node */

/* Check whatever all nodes were explored, is called after any new neighbor is found */
visit_all(_, _,Reachable):- 
	get_nodes(Nodes), 
	subtract(Nodes, Reachable, Diff), 
	is_empty(Diff).

/* Get all node neighors */
get_neighbors(Node, Neighbors):- 
	findall(Y, edge_curr(Node,Y), Left), 
	findall(Y, edge_curr(Y, Node), Right), 
	union(Left, Right, Neighbors).

/* Get single tree */
get_tree(Tree):- findall([X,Y], edge(X,Y), Ls), list_subset(Ls, Tree).

/* Get all nodes */
get_nodes(Nodes):- setof(X, node(X), Nodes).

/* Save all nodes to the database */
load_nodes:- 
	setof(X, Y^edge(X,Y), Ls), 
	setof(X, Y^edge(Y,X), Rs), 
	union(Ls, Rs, Nodes), 
	foreach(member(X, Nodes),assertz(node(X))). /* create dynamic predicate */

/* Preprocess each line to the Edge */
load_edge([X |[' ', Y]]):- X \= Y, not(edge(X,Y)),  assertz(edge(X,Y)).

/* Load graph edges */
load_edges([]).
load_edges([L|Ls]) :- load_edges(Ls), load_edge(L).

/* Check whatever list is empty */
is_empty(List):- not(member(_,List)).

/* Generate subset of input list */
list_subset([], []).
list_subset([X|T1], [X|T2]):- list_subset(T1, T2). /* no redo jump to enxt line skipping single element, afterward recurse deeper */
list_subset([_|T1], T2):- list_subset(T1, T2).

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