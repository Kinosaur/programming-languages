% Kaung Khant Lin ID: 6540131 Sec: 542
% Extension: 4 missionaries, 4 cannibals, boat capacity 3
% State: state(ML, CL, MR, CR, BoatSide)

% Step 1: Initial and goal (strict)
initial(state(4, 4, 0, 0, left)).
goal(state(0, 0, 4, 4, right)).

% Step 2: Valid state check (minimal, as before)
safe(state(ML, CL, MR, CR, _Boat)) :-
    ML >= 0, CL >= 0, MR >= 0, CR >= 0,
    (ML =:= 0 ; ML >= CL),
    (MR =:= 0 ; MR >= CR).

% Step 3: Boat moves with capacity 3 (at least 1 passenger)
% Generates all (M,C) with M+C in 1..3.
move(M, C) :-
    between(0, 3, M),
    between(0, 3, C),
    T is M + C,
    T >= 1, T =< 3.

% Step 4: State transition

% Boat on LEFT: send M missionaries, C cannibals left -> right
transition(state(ML, CL, MR, CR, left), state(ML2, CL2, MR2, CR2, right)) :-
    move(M, C),
    M =< ML, C =< CL,           % availability on left
    ML2 is ML - M, CL2 is CL - C,
    MR2 is MR + M, CR2 is CR + C,
    safe(state(ML2, CL2, MR2, CR2, right)).

% Boat on RIGHT: send M missionaries, C cannibals right -> left
transition(state(ML, CL, MR, CR, right), state(ML2, CL2, MR2, CR2, left)) :-
    move(M, C),
    M =< MR, C =< CR,           % availability on right
    MR2 is MR - M, CR2 is CR - C,
    ML2 is ML + M, CL2 is CL + C,
    safe(state(ML2, CL2, MR2, CR2, left)).

% Step 5: Path search with cycle checking (avoids infinite loops)
% Returns a list starting with Start and ending with Goal.
path(Start, Goal, [Start | PathTail]) :-
    dfs(Start, Goal, [Start], PathTail).

% Base: reached goal
dfs(Goal, Goal, _Visited, []) :- !.

% Step: expand to Next if not visited yet
dfs(Current, Goal, Visited, [Next | Rest]) :-
    transition(Current, Next),
    \+ member(Next, Visited),
    dfs(Next, Goal, [Next | Visited], Rest).

% Convenience
solve(Path) :-
    initial(S), goal(G),
    path(S, G, Path).