% Kaung Khant Lin ID: 6540131 542

% River crossing puzzle: Farmer, Fox, Goose, Beans
% The farmer must take all across safely using a boat that carries only him + one item.

% Bank switching
change(e,w).
change(w,e).

% Moves
% Farmer crosses with the fox
move([X,X,Goose,Beans],fox,[Y,Y,Goose,Beans]) :-
    change(X,Y).

% Farmer crosses with the goose
move([X,Fox,X,Beans],goose,[Y,Fox,Y,Beans]) :-
    change(X,Y).

% Farmer crosses with the beans
move([X,Fox,Goose,X],beans,[Y,Fox,Goose,Y]) :-
    change(X,Y).

% Farmer crosses alone
move([X,Fox,Goose,B],nothing,[Y,Fox,Goose,B]) :-
    change(X,Y).

% Safety check
% 1. Fox and goose cannot be left alone without farmer
% 2. Goose and beans cannot be left alone without farmer
safe([Farmer,Fox,Goose,Beans]) :-
    (Farmer = Goose ; Fox \= Goose),
    (Farmer = Goose ; Goose \= Beans).

% Solutions
solution([e,e,e,e],[]).

% Recursive case: apply a move, ensure safety, continue
solution(Config,[Move|Rest]) :-
    move(Config,Move,NextConfig),
    safe(NextConfig),
    solution(NextConfig,Rest).

% --- Example query ---
% ?- length(Moves,7), solution([w,w,w,w],Moves).
% Expected: Moves = [goose, nothing, fox, goose, beans, nothing, goose].
