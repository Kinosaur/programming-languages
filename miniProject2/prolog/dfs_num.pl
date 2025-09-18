:- use_module(library(lists)).
:- use_module(library(readutil)).
:- initialization(main, main).

% Run:
% swipl -q -s miniProject2/prolog/dfs_num.pl -- <input_file> <directed|undirected>

main :-
    current_prolog_flag(argv, [File, KindStr]),
    ( KindStr = directed ; KindStr = undirected ), !,
    ( run(File, KindStr) -> true ; true ),
    halt(0).
main :- format(user_error, "Usage: swipl -q -s dfs_num.pl -- <input_file> <directed|undirected>~n", []), halt(1).

run(File, Kind) :-
    read_file_to_string(File, Content, []),
    split_string(Content, "\n", "\r\t ", LinesRaw),
    exclude(=(""), LinesRaw, Lines),
    ( Lines = [NMLine, SrcDstLine | Rest] -> true
    ; writeln('No path'), !, fail
    ),
    parse_nm(NMLine, N, M),
    parse_pair_nums(SrcDstLine, Src, Dst),
    exclude(=(""), Rest, EdgeLinesAll),
    length(EdgeLinesAll, LAll),
    ( LAll < M -> writeln('No path'), !, fail ; true ),
    length(EdgeLines, M),
    append(EdgeLines, _, EdgeLinesAll),
    maplist(parse_edge_pair, EdgeLines, Edges0),
    normalize_edges(Edges0, Kind, Edges),
    build_adjacency(N, Edges, Adj0),
    sort_neighbors(Adj0, Adj),
    ( dfs(Adj, Src, Dst, Path) ->
        atomic_list_concat(Path, ' ', Line), writeln(Line)
    ; writeln('No path')
    ).

% parsing (same helpers as BFS)
parse_nm(Line, N, M) :-
    split_string(Line, " ", " ", Parts0),
    exclude(=(""), Parts0, [NS, MS]),
    number_string(N, NS), number_string(M, MS).

parse_pair_nums(Line, A, B) :-
    split_string(Line, " ", " ", Parts0),
    exclude(=(""), Parts0, [AS, BS]),
    number_string(A, AS), number_string(B, BS).

parse_edge_pair(Line, U-V) :- parse_pair_nums(Line, U, V).

normalize_edges(Edges, directed, Edges).
normalize_edges(Edges, undirected, Bi) :-
    findall(U-V, member(U-V, Edges), E1),
    findall(V-U, member(U-V, Edges), E2),
    append(E1, E2, Bi).

build_adjacency(N, Edges, Adj) :-
    numlist(1, N, Ns),
    maplist(init_node, Ns, Init),
    foldl(add_edge, Edges, Init, Adj1),
    true, Adj = Adj1.

init_node(U, U-[]).
add_edge(U-V, In, Out) :-
    select(U-Ns, In, Rest), !,
    Out = [U-[V|Ns] | Rest].
add_edge(_, In, In).

sort_neighbors(Adj0, Adj) :- maplist(sort_one, Adj0, Adj).
sort_one(U-Vs0, U-Vs) :- sort(Vs0, Vs).

neighbors(Adj, U, Ns) :- member(U-Ns, Adj).

% DFS: preorder mark (via Visited list), neighbors in numeric order
dfs(Adj, Src, Dst, Path) :-
    dfs_visit(Adj, Src, Dst, [Src], Rev), !,
    reverse(Rev, Path).

dfs_visit(_Adj, Dst, Dst, Acc, Acc) :- !.
dfs_visit(Adj, U, Dst, Vis, Path) :-
    neighbors(Adj, U, Ns),
    member(V, Ns),
    \+ member(V, Vis),
    dfs_visit(Adj, V, Dst, [V|Vis], Path), !.