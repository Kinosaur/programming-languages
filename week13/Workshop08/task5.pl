% Kaung Khant Lin ID: 6540131 542

% main predicate that builds valid arrangements based on the clues
solution(Street) :-
    Street = [house(C1, D1, P1), house(C2, D2, P2), house(C3, D3, P3)],

    % Colors, drinks and pets that will be used (no duplicates)
    Colors = [red, green, blue],
    Drinks = [tea, coffee, milk],
    Pets   = [cat, dog, fish],

    % assign colors, drinks and pets by permutation (simple for 3 items)
    permutation(Colors, [C1, C2, C3]),
    permutation(Drinks, [D1, D2, D3]),
    permutation(Pets,   [P1, P2, P3]),

    % Clues (translated):
    % 1) Red is immediately left of Green -> red is at position i and green at i+1
    adjacent_left(house(red, _, _), house(green, _, _), Street),

    % 2) Red-house person owns a cat
    member(house(red, _, cat), Street),

    % 3) Middle house drinks tea
    nth1(2, Street, house(_, tea, _)),

    % 4) Green-house person drinks coffee
    member(house(green, coffee, _), Street).

adjacent_left(A, B, [A, B, _]).
adjacent_left(A, B, [_, A, B]).

% Printing
cat_owner :-
    solution(Street),
    nth1(Pos, Street, house(Color, _, cat)),
    position_name(Pos, Name),
    format('Cat owner: ~w house (~w).~n', [Color, Name]).

coffee_drinker :-
    solution(Street),
    nth1(Pos, Street, house(Color, coffee, _)),
    position_name(Pos, Name),
    format('Coffee drinker: ~w house (~w).~n', [Color, Name]).

% Map position index to human name
position_name(1, left).
position_name(2, middle).
position_name(3, right).
