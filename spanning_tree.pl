input([['A', ' ', 'B'],['A', ' ', 'C'],['A', ' ', 'D'],['B', ' ', 'C'],['C', ' ', 'D']]).

:- dynamic edge/2, edge_curr/2, node/1.

write_edge([A,B]):- format('~w-~w', [A, B]).


write_line([]):- nl.
write_line([H|[]]):- write_edge(H), write_line([]).
write_line([H|T]):- write_edge(H), write(' '), write_line(T).

write_lines2([]).
write_lines2([H|T]) :- write_line(H), write_lines2(T). %(writeln je "knihovni funkce")

main :-
    prompt(_, ''),
    read_lines(LL),
    load_edges(LL),
    load_nodes,
	findall(Tree, (get_tree(Tree), is_spanning_tree(Tree)), Trees),
    % setof(Tree,generate_spanning_tree(Edges, Nodes, [], Tree),Trees),
    write_lines2(Trees),
    halt.

subset_l([], []).
subset_l([E|Tail], [E|NTail]):-
	subset_l(Tail, NTail).
subset_l([_|Tail], NTail):-
	subset_l(Tail, NTail).

is_spanning_tree(Edges):- 
	foreach(member([X, Y], Edges),assertz(edge_curr(X,Y))),
	node(Node), !, 
	(has_cycle(Node, -1, []) -> 
		(retractall(edge_curr(X,Y)), false); 
		(visit_all(Node, [], []) ->
			retractall(edge_curr(X,Y));
			(retractall(edge_curr(X,Y)), false))).

  
has_cycle(Curr, Prev, Visited):-
	subtract(Visited, [Prev], Vis),
	member(Curr, Vis) , !. 
has_cycle(Curr, Prev, Visited):-
	(edge_curr(Curr, Next); edge_curr(Next, Curr)),
	Next \= Prev,
	has_cycle(Next, Curr, [Curr|Visited]).

get_neighbors(Node, Neighbors):- findall(Y, edge_curr(Node,Y), Left), findall(Y, edge_curr(Y, Node), Right), union(Left, Right, Neighbors).


visit_all(Node, Visited, Result):-
	get_neighbors(Node,Neighbors), subtract(Neighbors, Visited, Can_visit), member(Next, Can_visit), union(Result, Neighbors, Reachable), visit_all(Next, [Node|Visited], [Node|Reachable]).	
visit_all(Node, Visited,Result):-get_nodes(Nodes), subtract(Nodes, Result, Diff), is_empty(Diff).


% explore_from_node([X,Y], [[X1,Y1]|Edges], Explored, Traversed_set):- (X == X1; X == Y1; Y==X1; Y== Y1; member(X1, Explored); member(Y1, Explored)), !, explore_from_node([X,Y], Edges, Explored, Traversed), flatten([[X1,Y1]|Traversed], Traversed_new), list_to_set(Traversed_new, Traversed_set).
% explore_from_node(Explore_from, [[_,_]|Edges], Explored, Traversed):- explore_from_node(Explore_from, Edges, Explored, Traversed).
% explore_from_node(_, [], _, []).


% common_list([X],[X]).
% common_list([X|Tail],Y):- member(X,Y).
% common_list([X|Tail],Y):- common_list(Tail,Y).

append_as_set(X, Y, R):- append(X,Y, S), list_to_set(S,R).


% explore_graph([Node|Nodes], Explored, Traversing_start, Traversed_next,Traversed_all):- !,append(Explored, [Node], New_explored), append(Explored, Nodes, To_be_explored), explore_from_node(Node,To_be_explored, Traversing_start, Traversed_new), append_as_set(Traversed_next, Traversed_new, Traversed), explore_graph(Nodes, New_explored, Traversing_start, Traversed,Traversed_all).
% explore_graph([], Explored, Traversing_start, Traversed_next, Traversed_next):- common_list(Traversing_start, Traversed_next), !.
% explore_graph([], Explored, Traversing_start, Traversed_next, Traversed_all):- explore_graph(Explored, [], Traversed_next, Traversed_next, Traversed_all).


get_tree(X):-findall([X,Y], edge(X,Y), Ls), subset_l(Ls, X).

% direct_neighbor(Node, [X,Y], Y):- Node = X.
% direct_neighbor(Node, [X,Y], X):- Node = Y.

% get_neighbors(Node, [], []):- !. 
% get_neighbors(Node, [Edge|Edges], [Neighbor|Neighbors]) :- direct_neighbor(Node, Edge, Neighbor), get_neighbors(Node, Edges, Neighbors).
% get_neighbors(Node, [_|Edges], Neighbors) :- get_neighbors(Node, Edges, Neighbors). 

% has_cycle(Node, -1, Nodes, Edges, Visited):- get_neighbors(Node, Edges, Neighbors), subtract(Neighbors, Parent, [Next_neighbor,New_neighbors]), intersection(New_neighbors, Visited, Common), is_empty(Common).
% has_cycle(Node, -1, Nodes, Edges, Visited):- get_neighbors(Node, Edges, Neighbors), subtract(Neighbors, Parent, New_neighbors), intersection(New_neighbors, Visited, Common), is_empty(Common).

% accesible_nodes(Node, Parent, Visited, Edges):-


% get_covered_nodes(Edges, Nodes):- 
% generate_spanning_tree([], Nodes, Buffer, Buffer):- explore_graph(Buffer, [], [], [], Accesible_nodes), subtract(Nodes, Accesible_nodes, Diff), is_empty(Diff).
% generate_spanning_tree([[X,Y]|Es], Nodes, Buffer, Tree):- flatten(Buffer, Flat), (not(member(X, Flat)); not(member(Y, Flat))), generate_spanning_tree(Es, Nodes, [[X,Y]| Buffer], Tree).
% generate_spanning_tree([_|Es], Nodes, Buffer, Tree):-  generate_spanning_tree(Es, Nodes, Buffer, Tree).



/* Check whatever list is empty */
is_empty(List):- not(member(_,List)).

/* Save all nodes to the database */
% load_nodes(Edges, Nodes):- flatten(Edges, Flat), list_to_set(Flat, Nodes).
load_nodes:- setof(X, Y^edge(X,Y), Ls), setof(X, Y^edge(Y,X), Rs), append_as_set(Ls, Rs, Nodes), foreach(member(X, Nodes),assertz(node(X))).


get_nodes(Nodes):- setof(X, node(X), Nodes).
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


/* Preprocess each line to the Edge */
% load_edge([X |[' ', Y]], [X,Y]):- X \= Y.
load_edge([X |[' ', Y]]):- X \= Y, not(edge(X,Y)),  assertz(edge(X,Y)).

/* Load graph edges */
load_edges([]).
load_edges([L|Ls]) :- load_edges(Ls), load_edge(L).

