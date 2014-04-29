
# Basics


## Project creation


Most popular clojure project creator is [leiningen](https://github.com/technomancy/leiningen), with commands like:

```bash
lein new seven-languages
;; Created new project in: seven-languages

cd seven-languages/
lein repl
;; Start an interactive shell
```


## Hello world


```clojure
(println "Hello world !")

(+ 1 1)
;; 2

(/ 2 4)
1/2

(/ 2.0 4)
0.5

(class (/ 1 3))
;; clojure.lang.Ratio
;; A ratio allow to use lazy calculation

(+ 2 2 2 2)
;; + is a function !!

(< 1 2 3)
;; true

(< 1 3 2 4)
;; false

(str 1)
% "1"
;; Will call toString if this is a Java class

;; A character is indicated by \
(str \f \o \r \c \e)
"force"
(= (str \a) "a")
true
```


## Boolean expressions


If expressions are like Io if:
```clojure
(if true (println "True it is."))
;; True it is.
;; nil

(if (> 1 2) (println "True it is."))
;; nil
```

**0** and **""** are true, but **nil** is not.


# Data containers


## List


In clojure, list are used as code. So if you want a list:

```clojure
(list 1 2 3)
;; Or
'(1 2 3)
```

Functions available with a list:

```clojure
(first '(:r2d2 :c3po))
;; :r2d2

(last '(:r2d2 :c3po))
;; :c3po

(rest '(:r2d2 :c3po))
;; (:c3po)

(cons :battle-droid '(:r2d2 :c3po))
;; (:battle-droid :r2d2 :c3po)
;; cons as combine :)
```


## Vectors


Vector is an ordered collection.
Vectors are optimized for random access.
Use lists for code and vectors for data.

```clojure
[:hutt :wookie :ewok]
;; [:hutt :wookie :ewok]

(first [:hutt :wookie :ewok])
;; :hutt

(nth [:hutt :wookie :ewok] 2)
;; :ewok

([:hutt :wookie :ewok] 2)
;; :ewok
;; A vector is also a function !

(concat [:darth-vader] [:darth-maul])
;; (:darth-vader :darth-maul)```
```

Basic functionnal programming operations are available:

```clojure
(first [:hutt :wookie :ewok])
;; :hutt

(rest [:hutt :wookie :ewok])
;; (:wookie :ewok)
```


## Set


A set is an unordered collection of elements.

```clojure
(def spacecraft #{:x-wing :y-wing :tie-fighter})
;; #{:x-wing :y-wing :tie-fighter}

(count spacecraft)
;; 3

(sort spacecraft)
;; (:tie-fighter :x-wing :y-wing)
```

We can create a sorted-set:

```clojure
(sorted-set 2 3 1)
;; #{1 2 3}
```

Operations between 2 sets:

```clojure
user=> (clojure.set/union #{:skywalker} #{:vader})
;; #{:skywalker :vader}

(clojure.set/difference #{1 2 3} #{2})
```

A vector is also a function (testing if an element is in vector):

```clojure
(#{:jar-jar :chewbacca} :chewbacca)
:chewbacca

(#{:jar-jar :chewbacca} :luke)
nil
```


## Map


```clojure
{:chewie :wookie :lea :human}
;; {:chewie :wookie, :lea :human}

{:darth-vader "obi wan", :luke "yoda"}
;; {:darth-vader "obi wan", :luke "yoda"}
;; Comma is available as a whitespace alternative

(assoc {:one 1} :two 2)
;; {:two 2, :one 1}
```

A sorted map:

```clojure
(sorted-map 1 :one, 3 :three, 2 :two)
;; {1 :one, 2 :two, 3 :three}
```

A keyword and a map are functions.

```
(def mentors {:darth-vader "obi wan", :luke "yoda"})

(mentors :luke)
;; "yoda"

(:luke mentors)
;; "yoda"
```

Merge between 2 maps:

```clojure
(merge {:y-wing 2, :x-wing 4} {:tie-fighter 2})
;; {:tie-fighter 2, :y-wing 2, :x-wing 4}

(merge-with + {:y-wing 2, :x-wing 4} {:tie-fighter 2 :x-wing 3})
;; {:tie-fighter 2, :y-wing 2, :x-wing 7}
```


## Sequence


Sequence is Clojure abstraction for all containers.

A sequence support many operations like first, rest, cons, etc.


# Functions


```clojure
(defn hey [name] (println (str "Hello " name)))

(defn add
    "An useless function !"
    [a, b]
    (+ a b))
;; With (doc add):
;; -------------------------
;; user/add
;; ([a b])
;;   An useless function !
;; nil
```


## Binding


```clojure
(defn line-end [[_ second]] second))
;; Destructuring is also available with function "let"
(defn line-end
    [lst]
    (let [[_ second] lst] second))

(def line [[0 0] [10 20]])
(line-end line)
;; [10 20]
```

A Hash can be destructured:

```clojure
(def person {:name "Jabba" :profession "Gangster"})

(let [{name :name} person] (str "The person's name is " name))
;; "The person's name is Jabba"
```


## Anonymous function


```clojure
(map (fn [w] (* 2 w)) [1 2])
;; (2 4)

;; We can also write:
;; #(* 2 %)
```


## Recursion


With JVM limitations about recursion, Clojure provides an efficiency syntax with **loop/recur**.

Syntax:

    (loop [x x-initial-value, y y-initial-value] (do-something-with x y))

Example:

```clojure
(defn size [v]
    (loop [l v, c 0]
    (if (empty? l)
        c
        (recur (rest l) (inc c)))))
```


# Advanced usages


## Predicates


A predicate function support a sequence:

```clojure
(every? number? [1 2 3 :four])
;; false
```


## List comprehensions


Can use a map and a filter in the same way:

```clojure
(for [x colors] (str "I like " x))
;; ("I like red" "I like blue")

;; Bind 2 lists with a filter
(for [x colors, y toys, :when (number? y)]
    (str "I like " x " " y "s"))
```


## Finite / Infinite sequences


A finite sequence can be built with a function like **range**:

```clojure
(range 1 10 3)
;; (1 4 7)
```

And an infinite with **cycle**:

```clojure
(take 5 (drop 2 (cycle [:lather :rinse :repeat])))
;; (:repeat :lather :rinse :repeat :lather)

;; Alternative syntax with ->> operator
(->> [:lather :rinse :repeat] (cycle) (drop 2) (take 5))
;; (:repeat :lather :rinse :repeat :lather)
```



