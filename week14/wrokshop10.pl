% Kaung Khant Lin ID: 6540131 Sec: 542
% State: state(ML, CL, MR, CR, BoatSide)

% Step 1 
initial(state(3, 3, 0, 0, left)).
goal(state(0, 0, 3, 3, right)).

% Step 2: validity of a state (minimal checks, as requested)
safe(state(ML, CL, MR, CR, _Boat)) :-
    ML >= 0, CL >= 0, MR >= 0, CR >= 0,
    (ML =:= 0 ; ML >= CL),
    (MR =:= 0 ; MR >= CR).

% Step 3: Boat moves (M = missionaries, C = cannibals)
% Kept simple; order nudged to start with (1,1).
move(1, 1).
move(1, 0).
move(0, 1).
move(2, 0).
move(0, 2).

% Step 4: State transition

% Boat is on the LEFT bank: left -> right.
transition(state(ML, CL, MR, CR, left), state(ML2, CL2, MR2, CR2, right)) :-
    move(M, C),            % choose a boat load
    M =< ML, C =< CL,      % availability on left bank
    ML2 is ML - M,         % subtract from left
    CL2 is CL - C,
    MR2 is MR + M,         % add to right
    CR2 is CR + C,
    safe(state(ML2, CL2, MR2, CR2, right)).

% Boat is on the RIGHT bank: right -> left.
transition(state(ML, CL, MR, CR, right), state(ML2, CL2, MR2, CR2, left)) :-
    move(M, C),
    M =< MR, C =< CR,      % availability on right bank
    MR2 is MR - M,         % subtract from right
    CR2 is CR - C,
    ML2 is ML + M,         % add to left
    CL2 is CL + C,
    safe(state(ML2, CL2, MR2, CR2, left)).

% Step 5: SIMPLE search (naive DFS) — may loop
% Base case: reached goal
path(S, S, [S]) :- !.
% Recursive case: take one transition and continue
path(S, G, [S | P]) :-
    transition(S, N),
    path(N, G, P).

% Example query (as in Step 6):
% ?- initial(S), goal(G), path(S, G, Path).