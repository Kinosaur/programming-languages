% Kaung Khant Lin ID: 6540131

edge(a, b).
edge(b, c).
edge(c, d).
edge(a, d).

path(X, Y) :- edge(X, Y).
path(X, Y) :- edge(X, Z), path(Z, Y).