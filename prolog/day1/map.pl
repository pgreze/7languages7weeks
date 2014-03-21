color(red).
color(green).
color(blue).

different(X, Y) :- \+(X = Y).

coloring(Alabama, Mississippi, Georgia, Tennessee, Florida) :-
  color(Alabama),
  color(Mississippi),
  color(Georgia),
  color(Tennessee),
  color(Florida),
  different(Mississippi, Tennessee),
  different(Mississippi, Alabama),
  different(Alabama, Tennessee),
  different(Alabama, Mississippi),
  different(Alabama, Georgia),
  different(Alabama, Florida),
  different(Georgia, Florida),
  different(Georgia, Tennessee).
