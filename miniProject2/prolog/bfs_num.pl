:- use_module(library(lists)).
:- use_module(library(assoc)).
:- use_module(library(readutil)).
:- initialization(main, main).

% Run:
%   swipl -q -s miniProject2/prolog/bfs_num.pl -- <input_file> <directed|undirected>
%
% Supports node labels that are numbers or arbitrary atoms (e.g., A, B, ...).

main :-
    current_prolog_flag(argv, [File, KindStr]),
    ( KindStr = directed ; KindStr = undirected ), !,
    ( run(File, KindStr) -> true ; true ),
    halt(0).
main :-
    format(user_error, "Usage: swipl -q -s bfs_num.pl -- <input_file> <directed|undirected>~n", []),
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

    % Collect exactly M edge lines (ignore extras)
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

    ( bfs(Adj, Src, Dst, Path) ->
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

sort_neighbors(Adj0, Adj) :-
    maplist(sort_one, Adj0, Adj).
sort_one(U-Vs0, U-Vs) :- sort(Vs0, Vs).

neighbors(Adj, U, Ns) :- member(U-Ns, Adj).

% ---------- BFS (difference-list queue implementation) ----------
bfs(Adj, Src, Dst, Path) :-
    empty_assoc(Visited0),
    put_assoc(Src, Visited0, true, Visited),
    empty_assoc(Parent0),
    Queue = [Src|Tail],
    bfs_q(Adj, Queue-Tail, Visited, Parent0, Dst, ParentOut, Found),
    Found == true,
    reconstruct(ParentOut, Src, Dst, Path).

bfs_q(_, QIn, Par, Par, _Dst, Par, false) :-
    QIn = []-[], !.
bfs_q(Adj, [U|Qs]-Tail, Vis, Par, Dst, ParOut, Found) :-
    ( U == Dst ->
        ParOut = Par, Found = true
    ; neighbors(Adj, U, Ns),
      enqueue_new(Ns, U, Qs-Tail, Qs1-Tail1, Vis, Vis1, Par, Par1),
      bfs_q(Adj, Qs1-Tail1, Vis1, Par1, Dst, ParOut, Found)
    ).

enqueue_new([], _U, DL, DL, Vis, Vis, Par, Par).
enqueue_new([V|Vs], U, QFront-QBack, QFrontOut-QBackOut, Vis0, Vis, Par0, Par) :-
    ( get_assoc(V, Vis0, _) ->
        QFront1 = QFront, QBack1 = QBack, Vis1 = Vis0, Par1 = Par0
    ;   put_assoc(V, Vis0, true, Vis1),
        put_assoc(V, Par0, U, Par1),
        QBack = [V|NewBack],
        QFront1 = QFront,
        QBack1 = NewBack
    ),
    enqueue_new(Vs, U, QFront1-QBack1, QFrontOut-QBackOut, Vis1, Vis, Par1, Par).

% ---------- Path reconstruction (already forward order) ----------
reconstruct(Parent, Src, Dst, Path) :-
    reconstruct_rev(Parent, Src, Dst, [], Path).

reconstruct_rev(_Parent, Src, Src, Acc, [Src|Acc]) :- !.
reconstruct_rev(Parent, Src, Node, Acc, Path) :-
    get_assoc(Node, Parent, Prev),
    reconstruct_rev(Parent, Src, Prev, [Node|Acc], Path).