fib(0, 0).
fib(X, Y) :- X > 0, fib(X, Y, _).

fib(1, 1, 0).
fib(X, Y1, Y2) :-
    X > 1,
    X1 is X - 1,
    fib(X1, Y2, Y3),
    Y1 is Y2 + Y3.

/* Fibonacci with "Fatal Error: local stack overflow" with fibonacci(50, What).
fibonacci(0,0).
fibonacci(1,1).
fibonacci(A,B) :-
    A > 1,
    A1 is A - 1,
    A2 is A - 2,
    fibonacci(A1,B1),
    fibonacci(A2,B2),
    B is B1 + B2.
*/
