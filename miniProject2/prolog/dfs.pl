:- module(dfs, [main/0]).

:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(assoc)).
:- use_module(library(readutil)).

% Auto-run when invoked as a script:
:- initialization(main, main).

% Usage:
% swipl -q -s miniProject2/prolog/dfs.pl -- <input.txt> <directed|undirected>

main :-
    current_prolog_flag(argv, Argv),
    ( Argv = [File, KindStr]
    -> true
    ;  usage, halt(1)
    ),
    ( (KindStr = directed ; KindStr = undirected) -> true ; usage, halt(1) ),
    ( catch(run(File, KindStr), _E, (writeln('No path'), halt(0)))
    -> true
    ;  true
    ),
    halt(0).

usage :-
    format(user_error,
           "Usage: swipl -q -s miniProject2/prolog/dfs.pl -- <input.txt> <directed|undirected>~n",
           []).

run(File, Kind) :-
    read_file_to_string(File, Content, []),
    split_string(Content, "\n", "\r\t ", RawLines),
    exclude(=(""), RawLines, Lines0),
    ( Lines0 = [First|Rest] -> true ; writeln('No path'), ! ),
    parse_pair(First, Src, Dst),
    maplist(parse_maybe_edge, Rest, EdgesMaybe),
    exclude(=(none), EdgesMaybe, Edges0),
    normalize_edges(Edges0, Kind, Edges1),
    build_adjacency(Edges1, Adj0),
    ensure_node(Adj0, Src, Adj1),
    ensure_node(Adj1, Dst, Adj2),
    sort_neighbors(Adj2, Adj),

    ( dfs_search(Adj, Src, Dst, Path)
    -> atomic_list_concat(Path, ' ', Line), writeln(Line)
    ;  writeln('No path')
    ).

% ---------- Parsing and graph building ----------

parse_pair(Line, A, B) :-
    split_string(Line, " ", " ", Parts0),
    exclude(=(""), Parts0, Parts),
    Parts = [Sa, Sb],
    atom_string(A, Sa),
    atom_string(B, Sb).

parse_maybe_edge(Line, U-V) :-
    Line \= "",
    parse_pair(Line, U, V), !.
parse_maybe_edge(_, none).

normalize_edges(Edges, directed, Edges).
normalize_edges(Edges, undirected, Bi) :-
    findall(U-V, member(U-V, Edges), E1),
    findall(V-U, member(U-V, Edges), E2),
    append(E1, E2, Bi).

build_adjacency(Edges, Adj) :-
    empty_assoc(Empty),
    foldl(add_edge, Edges, Empty, Assoc),
    assoc_to_list(Assoc, Pairs),
    maplist(pair_to_adj, Pairs, Adj).

add_edge(U-V, In, Out) :-
    ( get_assoc(U, In, Ns) -> NewNs = [V|Ns] ; NewNs = [V] ),
    put_assoc(U, In, NewNs, Out).

pair_to_adj(K-Vs, K-Vs).

ensure_node(Adj, Node, Out) :-
    ( member(Node-_, Adj) -> Out = Adj ; Out = [Node-[]|Adj] ).

sort_neighbors(Adj0, Adj) :-
    maplist(sort_one, Adj0, Adj).
sort_one(U-Vs0, U-Vs) :-
    maplist(atom, Vs0),
    sort(Vs0, Vs). % dedup + lexicographic

neighbors(Adj, U, Ns) :- ( member(U-Ns, Adj) -> true ; Ns = [] ).

% ---------- DFS (first-found path under sorted neighbor order) ----------

dfs_search(Adj, Src, Dst, Path) :-
    dfs_visit(Adj, Src, Dst, [Src], Rev), !,
    reverse(Rev, Path).

dfs_visit(_Adj, Dst, Dst, Acc, Acc) :- !.
dfs_visit(Adj, U, Dst, Visited, Path) :-
    neighbors(Adj, U, Ns),
    member(V, Ns),
    \+ member(V, Visited),
    dfs_visit(Adj, V, Dst, [V|Visited], Path), !.