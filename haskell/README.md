
# Basics


## Expressions and Primitive Types


Numbers and string:

```haskell
-- This is a comment

4 + 2.0 * 5
-- 14.0

4 * (5 + 1)
-- 24

"hello" ++ " world"
-- "hello world"

['a', 'b']
-- "ab"
```

Note: string is a list of character.

Bool:

```haskell
(5 + 5) /= 10
-- False

if (5 == 5) then "true" else "false"
-- "true"
```

Haskell is strongly typed, so no implicit cast are available:

```haskell
if 1 then "true" else "false"
--    No instance for (Num Bool)
--      arising from the literal '1'


"one" + 1
--     No instance for (Num [Char])
--       arising from a use of '+'
```


## Functions


Functions have 2 main parts:

- type declaration (optionnal)
- function declaration

```haskell
-- With the console, let assign
-- the function in local scope
let double x = x * 2

double 5
-- 10
```

Now in a file **double.hs**:

```haskell
module Main where

    -- In a haskell module, functions
    -- are declared as
    double :: Num -> Num
    double x = x + x
```

Note: Haskell uses **whitespace** to delimit **program blocks**, as Python.

Loading this file in console:

```haskell
:load double.hs
double 5

:t double
-- :t allows to know what inputs/outputs types
-- are used in double function
```

Recursion and pattern matching:

```haskell
-- In console
let fact x = if x == 0 then 1 else fact (x - 1) * x

-- In a file
module Main where
    factorial :: Integer -> Integer
    factorial 0 = 1
    factorial x = x * factorial (x - 1)
```

And now with a guard:

```haskell
module Main where
factorial :: Integer -> Integer
    factorial x
        | x > 1 = x * factorial (x - 1)
        | otherwise = 1
```


# Tuples and lists


## Basic examples

```haskell
reverse :: (String, String) -> (String, String)
reverse (s1, s2) = (s2, s1)
```

Now with head/tail list's functions:

```haskell
-- Always return second element
-- In a list
second = head . tail

-- equivalent to
second lst = head (tail lst)

:t second
-- second :: [c] -> c
```

Tuple has also helper functions like fst (return first element):

```haskell
last (String, String) -> String
last = fst . reverse
```

## Traversing lists


```haskell
-- Match list's head/tail
let h:t = [1, 2, 3, 4]
-- h is 1 and t is [2,3,4]

1:[2, 3]
-- [1,2,3]

[1]:[2, 3]
-- Error !! List are homogeneous.
-- Here, this is a list of Num.
-- This is haskell inference type system
-- in action.
```

zip combines list:

```haskell
zip "kirk" "spock"
[('kirk','spock')]
```

An example of tail recursion:

```haskell
-- Filter all event num in a list
allEven :: [Integer] -> [Integer]
allEven [] = []
allEven (h:t) = if even h then h:allEven t else allEven t
```

## Range


```haskell
[1..4]
-- [1,2,3,4]

[10..4]
-- []

-- But we can specify increment
-- by specifying the next item
[10, 8 .. 4]
-- [10,8,6,4]

take 5 [1..]
-- [1,2,3,4,5]
```


## List comprehension


```haskell
[x * 2 | x <- [1, 2, 3]]
-- [2,4,6]

let s = "abc"
[(a, b) | a <- s, b <- s, a < 'c' && b < 'c']
-- [('a','a'),('a','b'),('b','a'),('b','b')]
```

And with pattern matching:

```haskell
[ (y, x) | (x, y) <- [(1, 2), (2, 3), (3, 1)]]
```



