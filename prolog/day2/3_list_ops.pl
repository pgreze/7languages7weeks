% Compile with "['3_reverse.pl']."
% Test with "reverse2([1, 2, 3], What)."

reverse2([], L, L).
reverse2([Head|Tail], Reversed, L) :-
	reverse2(Tail, [Head|Reversed], L).
reverse2(X, Y) :- reverse2(X, [], Y).

min(X,Y,X) :- X<Y.
min(X,Y,Y) :- X>=Y.
minlist([X], X).
minlist([X|Tail], Min) :-
    minlist(Tail, Min1),
    min(X, Min1, Min).
