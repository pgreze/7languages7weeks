//
// First exercice
//

val strings = List("Hello", " ", "World", " ", "!!!")

// List of string size
println("size of string list is "+strings.foldLeft(0)((sum, str) => sum + str.size))

//
// Second exercice
//

trait Censor {
    //val words = Map(
    //    "shoot" -> "pucky",
    //    "darn" -> "beans"
    //)

    val definitions = io.Source.fromFile("censor_definition.csv").getLines mkString "\n"

    var words = Map[ String, String ]()
    definitions.split("\n").foreach(definition => {
        val terms = definition.split(",")
        words = words + (terms(0) -> terms(1))
    })

    def censor(content: String): String = {
        words.foldLeft(content)(
            (censoredContent, mapping) =>
                censoredContent.replaceAll(
                    ("(?i)\\b" + mapping._1 + "\\b"),
                    mapping._2
                )
        )
    }
}

class Witness(val name: String, val quote: String) extends Censor {
    def censoredQuote: String = {
        censor(quote)
    }
}

var p = new Witness("Obi-Wan Kenobi", "Some words were written in the wall: Shoot, Darn, Shoot, etc...")
println(p.name + " said:\n'"+ p.quote +"'")
println(p.name + " should have said:\n'"+ p.censoredQuote +"'")
