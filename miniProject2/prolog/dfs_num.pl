:- use_module(library(lists)).
:- use_module(library(readutil)).
:- initialization(main, main).

% Run:
%   swipl -q -s miniProject2/prolog/dfs_num.pl -- <input_file> <directed|undirected>
%
% Supports numeric or atom node labels.

main :-
    current_prolog_flag(argv, [File, KindStr]),
    ( KindStr = directed ; KindStr = undirected ), !,
    ( run(File, KindStr) -> true ; true ),
    halt(0).
main :-
    format(user_error, "Usage: swipl -q -s dfs_num.pl -- <input_file> <directed|undirected>~n", []),
    halt(1).

run(File, Kind) :-
    read_file_to_string(File, Content, []),
    split_string(Content, "\n", "\r\t ", LinesRaw),
    exclude(=(""), LinesRaw, Lines),
    ( Lines = [NMLine, SrcDstLine | Rest] -> true
    ; fail_no_path(parse_structure)
    ),
    parse_nm(NMLine, _DeclaredN, M),
    parse_pair_generic(SrcDstLine, Src, Dst),

    exclude(=(""), Rest, EdgeLinesAll),
    length(EdgeLinesAll, LAll),
    ( LAll < M -> fail_no_path(too_few_edges) ; true ),
    length(EdgeLines, M),
    append(EdgeLines, _, EdgeLinesAll),
    maplist(parse_edge_pair_generic, EdgeLines, EdgePairs0),
    normalize_edges(EdgePairs0, Kind, Edges),

    collect_all_nodes(Edges, Src, Dst, NodeSet),
    ( member(Src, NodeSet), member(Dst, NodeSet)
      -> true
      ;  fail_no_path(src_or_dst_missing)
    ),

    build_adjacency(NodeSet, Edges, Adj0),
    sort_neighbors(Adj0, Adj),

    ( dfs(Adj, Src, Dst, Path) ->
        atomic_list_concat(Path, ' ', Line),
        writeln(Line)
    ; fail_no_path(no_search_path)
    ).

fail_no_path(_Reason) :-
    writeln('No path'), !, fail.

% ---------- parsing ----------
parse_nm(Line, N, M) :-
    split_string(Line, " ", " ", Parts0),
    exclude(=(""), Parts0, [NS, MS]),
    number_string(N, NS),
    number_string(M, MS).

parse_pair_generic(Line, A, B) :-
    split_string(Line, " ", " ", Parts0),
    exclude(=(""), Parts0, [AS, BS]),
    parse_node(AS, A),
    parse_node(BS, B).

parse_edge_pair_generic(Line, U-V) :-
    parse_pair_generic(Line, U, V).

parse_node(Str, Node) :-
    ( number_string(N, Str) ->
        Node = N
    ; atom_string(Node, Str)
    ).

normalize_edges(Edges, directed, Edges).
normalize_edges(Edges, undirected, Bi) :-
    findall(U-V, member(U-V, Edges), E1),
    findall(V-U, member(U-V, Edges), E2),
    append(E1, E2, Bi).

collect_all_nodes(Edges, Src, Dst, Nodes) :-
    findall(X, (member(A-B, Edges), (X=A ; X=B)), Flat),
    append(Flat, [Src, Dst], All),
    sort(All, Nodes).

% ---------- adjacency ----------
build_adjacency(Nodes, Edges, Adj) :-
    maplist(init_node, Nodes, Init),
    foldl(add_edge, Edges, Init, Adj1),
    Adj = Adj1.

init_node(U, U-[]).
add_edge(U-V, In, Out) :-
    select(U-Ns, In, Rest), !,
    Out = [U-[V|Ns] | Rest].
add_edge(_, In, In).

sort_neighbors(Adj0, Adj) :- maplist(sort_one, Adj0, Adj).
sort_one(U-Vs0, U-Vs) :- sort(Vs0, Vs).

neighbors(Adj, U, Ns) :- member(U-Ns, Adj).

% ---------- DFS (forward path accumulation) ----------
dfs(Adj, Src, Dst, Path) :-
    dfs_visit(Adj, Src, Dst, [Src], Path).

% dfs_visit(+Adj, +Current, +Dst, +PathSoFarForward, -FinalPath)
dfs_visit(_Adj, Dst, Dst, Path, Path) :- !.
dfs_visit(Adj, U, Dst, PathSoFar, Path) :-
    neighbors(Adj, U, Ns),
    member(V, Ns),
    \+ member(V, PathSoFar),
    append(PathSoFar, [V], PathNext),          % extend forward
    dfs_visit(Adj, V, Dst, PathNext, Path), !.
