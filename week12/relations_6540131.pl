% Facts
male(kim).
male(kent).
male(herbert).
female(holly).
female(margaret).
female(esther).
female(jean).

parent(kim, holly).
parent(margaret, kim).
parent(margaret, kent).
parent(esther, margaret).
parent(herbert, margaret).
parent(herbert, jean).

% 1. mother(X,Y) means X is the mother of Y
mother(X, Y) :-
    female(X),
    parent(X, Y).

% 2. father(X,Y) means X is the father of Y
father(X, Y) :-
    male(X),
    parent(X, Y).

% 3. sister(X,Y) means X is a sister of Y
sister(X, Y) :-
    female(X),
    parent(Z, X),
    parent(Z, Y),
    X \= Y.

% 4. brother(X,Y) means X is a brother of Y
brother(X, Y) :-
    male(X),
    parent(Z, X),
    parent(Z, Y),
    X \= Y.

% 5. sibling(X,Y) means X and Y are siblings
sibling(X, Y) :-
    parent(Z, X),
    parent(Z, Y),
    X \= Y.

% 6. cousin(X,Y) means X and Y are cousins if their parents are siblings
cousin(X, Y) :-
    parent(P1, X),
    parent(P2, Y),
    sibling(P1, P2),
    X \= Y.

% 7. niece(X,Y) means X is the niece of Y
niece(X, Y) :-
    female(X),
    parent(P, X),
    sibling(P, Y).

% 8. nephew(X,Y) means X is the nephew of Y
nephew(X, Y) :-
    male(X),
    parent(P, X),
    sibling(P, Y).

