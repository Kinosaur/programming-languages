:- use_module(library(lists)).
:- use_module(library(assoc)).
:- use_module(library(readutil)).
:- initialization(main, main).

% Run:
% swipl -q -s miniProject2/prolog/bfs_num.pl -- <input_file> <directed|undirected>

main :-
    current_prolog_flag(argv, [File, KindStr]),
    ( KindStr = directed ; KindStr = undirected ), !,
    ( run(File, KindStr) -> true ; true ),
    halt(0).
main :- format(user_error, "Usage: swipl -q -s bfs_num.pl -- <input_file> <directed|undirected>~n", []), halt(1).

run(File, Kind) :-
    read_file_to_string(File, Content, []),
    % split on newlines; trim CR, tabs, and spaces
    split_string(Content, "\n", "\r\t ", LinesRaw),
    exclude(=(""), LinesRaw, Lines),
    ( Lines = [NMLine, SrcDstLine | Rest] -> true
    ; writeln('No path'), !, fail
    ),
    parse_nm(NMLine, N, M),
    parse_pair_nums(SrcDstLine, Src, Dst),
    % take exactly M non-empty edge lines
    exclude(=(""), Rest, EdgeLinesAll),
    length(EdgeLinesAll, LAll),
    ( LAll < M -> writeln('No path'), !, fail ; true ),
    length(EdgeLines, M),
    append(EdgeLines, _, EdgeLinesAll),
    maplist(parse_edge_pair, EdgeLines, Edges0),
    normalize_edges(Edges0, Kind, Edges),
    build_adjacency(N, Edges, Adj0),
    sort_neighbors(Adj0, Adj),
    ( bfs(Adj, Src, Dst, Path) ->
        atomic_list_concat(Path, ' ', Line), writeln(Line)
    ; writeln('No path')
    ).

% ---------- parsing ----------
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

% ---------- adjacency (nodes 1..N always present) ----------
build_adjacency(N, Edges, Adj) :-
    numlist(1, N, Ns),
    maplist(init_node, Ns, Init),
    foldl(add_edge, Edges, Init, Adj1),
    true, Adj = Adj1.

init_node(U, U-[]).

add_edge(U-V, In, Out) :-
    % put V into U s neighbor list (dedup later)
    select(U-Ns, In, Rest), !,
    Out = [U-[V|Ns] | Rest].
add_edge(_, In, In).

sort_neighbors(Adj0, Adj) :-
    maplist(sort_one, Adj0, Adj).
sort_one(U-Vs0, U-Vs) :- sort(Vs0, Vs).

neighbors(Adj, U, Ns) :- member(U-Ns, Adj).

% ---------- BFS: visited at enqueue; stop when Dst is dequeued ----------
bfs(Adj, Src, Dst, Path) :-
    empty_assoc(Visited0),
    put_assoc(Src, Visited0, true, Visited),
    empty_assoc(Parent0),
    bfs_q(Adj, [Src], Visited, Parent0, Dst, ParentOut, Found),
    Found == true,
    reconstruct(ParentOut, Src, Dst, Path).

bfs_q(_, [], Par, Par, _, Par, false) :- !.
bfs_q(Adj, [U|Qs], Vis, Par, Dst, ParOut, Found) :-
    ( U =:= Dst ->
        ParOut = Par, Found = true
    ; neighbors(Adj, U, Ns),
      enqueue_new(Ns, U, Qs, Vis, Vis1, Par, Par1, Qs1),
      bfs_q(Adj, Qs1, Vis1, Par1, Dst, ParOut, Found)
    ).

enqueue_new([], _U, Q, Vis, Vis, Par, Par, Q).
enqueue_new([V|Vs], U, Q0, Vis0, Vis, Par0, Par, Q) :-
    ( get_assoc(V, Vis0, _) ->
        Q1 = Q0, Vis1 = Vis0, Par1 = Par0
    ;   put_assoc(V, Vis0, true, Vis1),
        put_assoc(V, Par0, U, Par1),
        append(Q0, [V], Q1)
    ),
    enqueue_new(Vs, U, Q1, Vis1, Vis, Par1, Par, Q).

reconstruct(Parent, Src, Dst, Path) :-
    ( Dst =:= Src -> Path = [Src]
    ; get_assoc(Dst, Parent, Prev)
      -> reconstruct(Parent, Src, Prev, Pref),
         append(Pref, [Dst], Path)
      ;  fail
    ).