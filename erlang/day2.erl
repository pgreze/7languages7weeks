-module(day2).
-export([searchPredicat/2, totalPricePerProducts/1, boardStatus/1]).

% Fight = [{winner, "Chuck Norris"}, {loser, "Jean-Claude Van Damme"}].
% day2:searchPredicat(Fight, winner).

% With list comprehension but return a list:
% searchPredicat(Tuples, SearchKey) -> [Value || {Predicat, Value} <- Tuples, Predicat == SearchKey].

searchPredicat([{Key,Value}|Tail], Predicat) -> case Key of
    Predicat -> Value;
    _ -> searchPredicat(Tail, Predicat)
end;
searchPredicat([], _) -> null.

% Products = [{"Car", 1, 10000}, {"Vegetables", 10, 5}].
% day2:totalPricePerProducts(Products).
% Will return [{"Car",10000},{"Vegetables",50}]
totalPricePerProducts(Products) -> [{Item, Quantity * Price} || {Item, Quantity, Price} <- Products].


%
% Tic Tac Toe status board
%
% day2:boardStatus([" "," "," "," "," "," "," "," "," "]).
% no_winner
% day2:boardStatus([" "," "," ","X","X","X"," "," "," "]).
% X
% day2:boardStatus(["X","O","X","O","X","O","O","X","O"]).
% cat

% Entry point
boardStatus(Board) -> boardStatusWithSolutions(Board, [
    [1,2,3], [4,5,6], [7,8,9],
    [1,4,7], [2,5,8], [3,6,9],
    [1,5,9], [3,5,7]
], false).
% Resolve board with iteration over available solutions.
boardStatusWithSolutions(_, [], AvailableCellFound) -> case AvailableCellFound of
    false -> cat;
    _ -> no_winner
end;
boardStatusWithSolutions(Board, [Solution|Tail], AvailableCellFound) ->
    % Take nth element in list (begin at 1)
    % For all index in current solution resolution
    Cells = [string:strip(lists:nth(X, Board)) || X <- Solution],
    PlayedCells = [X || X <- Cells, X /= ""],
    case PlayedCells of
        % Stop when a winner is found
        [X,X,X] -> X;
        % Else continue with next solution
        _ -> boardStatusWithSolutions(
            Board, Tail,
            % If no available cell, search at least one empty cell in current values
            (AvailableCellFound or lists:any(fun(X) -> X == "" end, Cells))
        )
    end
.
