% Basic functions. Used with:
% 1> c(basic).
% {ok,basic}
% 2> basic:counts("hello world").
% 11
-module(basic).
-export([mirror/1, counts/1, ten/0, errprint/1]).

mirror(Anything) -> Anything.

% Write a function that uses recursion to return the number of words in a string
%count([]) -> 0;
%counts([_|Tail]) -> 1 + counts(Tail).
counts(String) -> string:len(String).

% Write a function that uses recursion to count to ten.
ten2(10) -> 10;
ten2(X) -> ten2(X + 1).
ten() -> ten2(0).

% Write a function that uses matching to selectively print
% “success” or “error: message”
% given input of the form {error, Message} or success.
errprint(success) -> "success";
errprint({error, Message}) -> string:concat("error: ", Message).
