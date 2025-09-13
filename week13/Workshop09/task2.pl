% Kaung Khant Lin ID: 6540131 542

% Shape predicates

shape(a, circle).
shape(b, circle).
shape(c, square).
shape(d, triangle).
shape(e, square).
shape(f, triangle).
shape(g, square).
shape(h, square).
shape(i, triangle).
shape(j, square).

% Color predicates

color(a, blue).
color(b, grey).
color(c, blue).
color(d, grey).
color(e, grey).
color(f, blue).
color(g, grey).
color(h, grey).
color(i, blue).
color(j, blue).

% Position predicates (row, column)

pos(a, 1, 3).
pos(b, 2, 1).
pos(c, 2, 2).
pos(d, 2, 3).
pos(e, 3, 1).
pos(f, 3, 2).
pos(g, 3, 3).
pos(h, 4, 1).
pos(i, 4, 3).
pos(j, 4, 4).

rightOf(X, Y) :-
    pos(X, _, X_col),
    pos(Y, _, Y_col),
    X_col > Y_col.

% Helper predicates

all_items_of_shape(Shape, ListOfItems) :-
    findall(Item, shape(Item, Shape), ListOfItems).

all_items_of_color(Color, ListOfItems) :-
    findall(Item, color(Item, Color), ListOfItems).

all_items_of_type(Predicate, Value, ListOfItems) :-
    findall(Item, call(Predicate, Item, Value), ListOfItems).