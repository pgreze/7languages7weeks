// Can be used by scala or scalac
object Main {
    def printArgs(args: Array[String]) {
        println("Arguments:")
        var i = 1
        args.foreach { arg =>
            println(i + ": " + arg)
            i += 1
        }
        // Iterative alternative
        //for(i <- 0 to args.size-1) {
        //    println((i+1) + ": " + args(i))
        //}
    }

    def main(args: Array[String]) {
        if (args.size > 0) {
            printArgs(args)
        } else {
            println("Give me arguments !!")
        }
    }
}