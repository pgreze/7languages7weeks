
# Main features


## Use scala


**scala** is a Scala interpreter, and can execute a file
without Object declaration (not like **scalac**).


## Range


A Range(start, end, step) class is available :

```scala
scala> val range = 0 until 10
range: Range = Range(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
scala> range.start
res2: Int = 0
scala> range.end
res3: Int = 10
scala> range.step
res4: Int = 1

scala> (0 to 10) by 5
res6: Range = Range(0, 5, 10)
scala> (0 to 10) by 6
res7: Range = Range(0, 6)

scala> val range = (10 until 0) by -1
range: Range = Range(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)

scala> val range = 'a' to 'e'
range: RandomAccessSeq.Projection[Char] = RandomAccessSeq.Projection(a, b, c, d, e)
```


## Tuples


```scala
scala> val person = ("Elvis", "Presley")
person: (java.lang.String, java.lang.String) = (Elvis,Presley)
scala> person._1
res9: java.lang.String = Elvis
scala> person._2
res10: java.lang.String = Presley
```

Can be used for multivalue assignments:

```scala
val (x, y) = (1, 2)
```


## Classes


### Empty class and many constructors


```scala
// Default class with no methods
class Person(firstName: String, lastName: String)

// Initialisation
val gump = new Person("Forrest", "Gump")

class Person(first_name: String) {
    // Default constructor begin
    println("Outer constructor")

    // Alternative constructor with specific behavior
    // Called when 2 parameters are given
    // BUT default constructor IS ALSO CALLED !!
    def this(first_name: String, last_name: String) {
        this(first_name)
        println("Inner constructor")
    }

    // Simple method
    def talk() = println("Hi")

    def talk(message: String) {
        println("you said: " + message)
    }
}

// Print:
// Outer constructor
val bob = new Person("Bob")
// Print:
// Outer constructor
// Inner constructor
val bobTate = new Person("Bob", "Tate")
```



### Singleton


```scala
object TrueRing {
    println("Hello")
    def rule = println("To rule them all")
}

// Use singleton instance
// Display:
// Hello
// To rule them all
TrueRing.rule

// Second time with display:
// To rule them all
TrueRing.rule
```


### Inheritance


```scala
class Person(val name: String) {
    def talk(message: String) = println(name + " says " + message)
    def id(): String = name
}

// Employee inherits from Person
class Employee(override val name: String,
                        val number: Int) extends Person(name) {
    override def talk(message: String) {
        println(name + " with number " + number + " says " + message)
    }
    override def id():String = number.toString
}
```


### Traits


Traits is like a Ruby mixin or Java interface + an implementation.

```scala
class Person(val name:String)

trait Nice {
    def greet() = println("Howdily doodily.")
}

trait Lier {
    def lie() = println("I love you.")
}

class Character(override val name:String)
    extends Person(name) with Nice with Lier

val flanders = new Character("Ned")
flanders.greet
flanders.lie
```


## Collections


Scala supports three collections: lists, maps, and sets. 


### Set


```scala
scala> val animals = Set("lions", "tigers", "bears") animals: scala.collection.immutable.Set[java.lang.String] =
    Set(lions, tigers, bears)
scala> animals + "armadillos"
res25: scala.collection.immutable.Set[java.lang.String] =
    Set(lions, tigers, bears, armadillos)
scala> animals - "tigers"
res26: scala.collection.immutable.Set[java.lang.String] = Set(lions, bears)

// Union / difference
scala> animals ++ Set("armadillos", "raccoons")
res28: scala.collection.immutable.Set[java.lang.String] =
  Set(bears, tigers, armadillos, raccoons, lions)
scala> animals -- Set("lions", "bears")
res29: scala.collection.immutable.Set[java.lang.String] = Set(tigers)

// Intersection
scala> animals ** Set("armadillos", "raccoons", "lions", "tigers")
res1: scala.collection.immutable.Set[java.lang.String] = Set(lions, tigers)
```


### Map


```scala
scala> val ordinals = Map(0 -> "zero", 1 -> "one", 2 -> "two") ordinals: scala.collection.immutable.Map[Int,java.lang.String] =
  Map(0 -> zero, 1 -> one, 2 -> two)
scala> ordinals(2)
res41: java.lang.String = two
```

With Mutable Map :

```scala
scala> import scala.collection.mutable.HashMap
scala> val map = new HashMap[Int, String]
map: scala.collection.mutable.HashMap[Int,String] = Map()
scala> map += 4 -> "four"
scala> map += 8 -> "eight"
```


### Functions applied to collections


With foreach, a **List** return contained element's type:

```scala
scala> list.foreach(hobbit => println(hobbit)) frodo
samwise
pippin
```

And a **Map** return a **Tuple**:

```scala
scala> hobbits.foreach(hobbit => println(hobbit._1)) frodo
samwise
pippin
```

Like Javascript, many functions are available with a **List**:

```scala
scala> words.count(word => word.size > 2)
res43: Int = 3
scala> words.filter(word => word.size > 2)
res44: List[java.lang.String] = List(peg, bud, kelly)
scala> words.map(word => word.size)
res45: List[Int] = List(3, 2, 3, 5)
scala> words.forall(word => word.size > 1)
res46: Boolean = true
scala> words.exists(word => word.size > 4)
res47: Boolean = true
scala> words.exists(word => word.size > 5)
res48: Boolean = false
```

**Reduce** syntaxic sugar:

```scala
scala> val list = List(1, 2, 3)
list: List[Int] = List(1, 2, 3)
scala> val sum = (0 /: list) {(sum, i) => sum + i}
sum: Int = 6
// Equivalent :
scala> list.foldLeft(0)((sum, value) => sum + value)
res54: Int = 6
```



## Divers


- **Var** create mutable object (updatable). **Val** create an imutable object.

- **Any** is the catchall data type for Scala. Example :

```scala
scala> List("one", "two", 3)
res6: List[Any] = List(one, two, 3)
```

- 