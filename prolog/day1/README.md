# Terminology

- An **atom** (unique symbol) begins with a lowercase character.
- A **variable** (can change) begins with an uppercase letter / an underscore.

# Howto

Define facts:

```prolog
likes(wallace, cheese).
likes(grommit, cheese).
likes(wendolene, sheep).
```

Define a rule:

```prolog
friend(X, Y) :- \+(X = Y), likes(X, Z), likes(Y, Z).
```

That say: X is a friend of Y if:

- X != Y
- and X likes Z
- and Y likes Z

Now, we can load this code in prolog interpreter and tell our questions:

```prolog
| ?- ['friends.pl'].
| ?- likes(wallace, sheep).
no
| ?- likes(grommit, cheese).
yes
```

We can also found **Who** like cheese (prolog detect that **Who** is a variable and try to assign it with a solution):

```prolog
| ?- likes(Who, cheese).

Who = wallace ? h
Action (; for next solution, a for all solutions, RET to stop) ? ;

Who = grommit ? 

(1 ms) yes
| ?- 
```

I have pressed "h" for help, ";" and finally "RET" to stop.

# Bonus

With map.pl, you can choose an unique color for each provinces put as coloring argument.

Execution example:

```prolog
| ?- ['map.pl'].
...
(1 ms) yes
| ?- coloring(red, Mississippi, Georgia, Tennessee, Florida).   

Florida = blue
Georgia = green
Mississippi = green
Tennessee = blue ? 
```

