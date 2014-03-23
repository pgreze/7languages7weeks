father(zeb, john_boy_sr).
father(john_boy_sr, john_boy_jr).

% Avoid infinite recursive call with simple fact: a father is an ancestor
ancestor(X, Y) :-
    father(X, Y).
% X is an ancestor of Y if X is father of Z and Z an ancestor of Y
ancestor(X, Y) :-
    father(X, Z), ancestor(Z, Y).
