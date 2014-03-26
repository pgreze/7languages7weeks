
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


