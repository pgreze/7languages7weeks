

# Basics


## Presentation


Erlang interpreter is **erl**.

Erlang source extension is **.erl** and compiled extension is **.beam**.

[stdlib documentation](http://www.erlang.org/doc/man/STDLIB_app.html) and [an alternative](http://erldocs.com/17.0-rc1/index.html?i=724#stdlib).


## Basic examples


Some basic examples :

```erlang
% Compute 2 integers addition
2 + 2.         % Result: 4

% Return a float
2 + 2.0.       % Result: 4.0

% Integer + string => error
4 + "string".
% ** exception error: bad argument in an arithmetic expression
%  in operator  +/2
%     called as 4 + "string"

% Variables must start with an uppercase letter.
Var = 1.
Var = 2.       % Error: variables are immutable

% Atom is an Erlang atomic identifier.
Color = blue.
Color != red.
```


# Containers


## Tuples


Erlang support tuple:

```erlang
Origin = {0, 0}.
```

Tuples can be used as hashmap:

```erlang
Comic = {comic_strip, {name, "Calvin and Hobbes"}, {character, "Spaceman Spiff"}}.

% Match Person and Profession
{person, {name, Name}, {profession, Profession}} = Person.

Name.              % Result: "Agent Smith"
Profession.        % Result: "Killing programs"
```


## List


List declaration:

```erlang
% List declaration
[1, 2, 3].
```

List pattern matching is like Prolog:

```erlang
[Head | Tail] = [1, 2, 3].

% With more than 2 variables
[One, Two|Rest] = [1, 2, 3].
```

An error is raised if list has not enough elements:

```erlang
[X|Rest] = [].
% ** exception error: no match of right hand side value []
```


### List operations


lists package allows to use several functions like **lists:foreach**:

```erlang
Numbers = [1, 2, 3]

% Define function
Print = fun(X) -> io:format("~p~n", [X]) end.
% Return nothing
lists:foreach(Print, Numbers).
```

**lists:map**:

```erlang
% Will return [2, 3, 4]
lists:map(fun(X) -> X + 1 end, Numbers).
```

**lists:filter**:

```erlang
% Will return [2, 3]
lists:filter(fun(X) -> X < 4 end, Numbers).
```

Other functions are also available:

- **lists:any(Func, List)**
- **lists:all(Func, List)**
- **lists:takewhile(Func, List).** (take elements since Func is true)
- **lists:dropwhile(Func, List).** (take elements when Func is true)
- **lists:foldl(Func, InitialSum, Numbers).** is another name for reduce function in javascript.

But Erlang provides also list comprehension :

```erlang
% Will return [2,2,4,6,10]
[Double(X) || X <- [1,1,2,3,5]].

% Another syntax
[X * 2 || X <- Fibs].

% Applied with list of tuples
Cart = [{pencil, 4, 0.25}, {pen, 1, 1.20}, {paper, 2, 0.20}].
WithTax = [{Product, Quantity, Price, Price * Quantity * 0.08} ||
    {Product, Quantity, Price} <- Cart].
% Return [{pencil,4,0.25,0.08},{pen,1,1.2,0.096},{paper,2,0.2,0.032}]
```

A list comprehension begins with an expression and many clauses, Which add or remove elements. An example:

```erlang
[{X,Y}||X <-[1,2,3,4],X<3,Y<-[5,6]].
% Return [{1,5},{1,6},{2,5},{2,6}]
```


## Bit matching


Erlang has an easy syntax to manage bit operations:

```erlang
W = 1.
X = 2.
Y = 3.
Z = 4.
All = <<W:3, X:3, Y:5, Z:5>>.
All.   % Return: "(d"
```

Reverse matching:

```erlang
<<A:3, B:3, C:5, D:5>> = All.
A.     % Result: 1
D.     % Result: 4
```


# Functions


File example:

```erlang
-module(basic).               % Defines the name of the module
-export([mirror/1]).          % Defines an usable function with /1 parameter

mirror(Anything) -> Anything. % Function declaration
```

Compiled and used in erlang interpreter:

```erlang
1> c(basic).
{ok,basic}
2> basic:mirror(smiling_mug).
smiling_mug
```


Function declaration can used arguments matching:

```erlang
number(one)   -> 1;
number(two)   -> 2;
number(three) -> 3.
```


## Anonymous function


```erlang
Negate = fun(I) -> -I end.
```


# Control structures (if, case)


Case:

```erlang
Animal = "dog".
case Animal of
    "dog" -> underdog;
    "cat" -> thundercat;
    _ -> something_else
end.
```

If:

```erlang
if
    X > 0 -> positive;
    X < 0 -> negative;
    % an exception is raised if no condition are true
    % so with "true ->", we avoid this
    true -> zero
end.
```


# Concurrency primitives


## Basics


There are 3 primitives to play with concurrency in Erlang :

- **receive** to receive a message
- *Pid = **spawn**(fun translate:loop/0).* to spawn a function translate:loop as a lightweight process.
- *Pid **!** Message* to send a message to the process.

There is a basic example of **translate:loop** :

```erlang
-module(translate).
-export([loop/0]).
loop() ->
    receive
        "casa" ->
            io:format("house~n"),
            loop();
        _ ->
            io:format("I don't understand.~n"),
            loop()
    end.
```


## Synchrone communication


To communicate between 2 process, we can send a tuple message with requester's Pid and read message. With our translation system:

```erlang
receive
    {Pid, "casa"} ->
        Pid ! "house",
        loop();
    ...
```

And in requester, with a new function:

```erlang
translate(To, Word) ->
    To ! {self(), Word},
    receive
        Translation -> Translation
    end.
```


## Terminate a process


A process can terminate with a message:

```erlang
receive
    exit_msg -> exit({process,die,at,erlang:time()});
```

A function can indicated if a process is die:

```erlang
erlang:is_process_alive(Pid).
```

## Monitoring a process


Erlang allow to create monitoring process, with the function **link** and can receive an exit message. After that, the monitor can respawn a died process or do additionnal actions. An example:

```erlang
loop() ->
    % Allow the process to receive EXIT message
    process_flag(trap_exit, true),

    % Receive loop
    receive
        new ->
            io:format("Creating and monitoring process.~n"),
            % spawn and link a process at the same time
            % Allow also to use revolver as the process' global name
            register(revolver, spawn_link(fun roulette:loop/0)),
            loop();

        % EXIT with process's pid and reason
        % is the tuple message sent by the exit function
        % to all process registered to trap_exit
        {'EXIT', From, Reason} ->
            io:format("The shooter ~p died with reason ~p.", [From, Reason]),
            io:format(" Restarting. ~n"),
            % Sending a message to ourself,
            % to spawn and register a new process
            self() ! new,
            loop()
        end.
```

OTP provides very great process's supervisors with many monitoring policies.
See this [good tutorial](http://learnyousomeerlang.com/supervisors).

