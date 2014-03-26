import scala.util.Random
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map

object Board {

    val choices = Map[String, Integer](
        "A1" -> 0,
        "B1" -> 1,
        "C1" -> 2,
        "A2" -> 3,
        "B2" -> 4,
        "C2" -> 5,
        "A3" -> 6,
        "B3" -> 7,
        "C3" -> 8
    )
    var moves = Map[Integer, String]()

    val rand = new Random(System.currentTimeMillis())

    def play {
        var winner = ""
        var step = 1

        draw

        while(winner == "" && step < 6) {
            val user_choice = user_interact(step)

            moves(user_choice) = "X"
            if(step < 5)
                moves(ia_choice) = "O"

            draw

            winner = get_winner
            step += 1
        }

        if(winner == "X")
            println("You win !!")
        else {
            println("You failed !!")
        }
    }

    def get_winner: String = {
        var winner = ""
        for(i <- 0 to 2) {
            // Vertical lines
            winner = get_winner_for_line(i, i+3, i+6)
            if(winner != "")
                return winner;
            // Horizontal lines
            winner = get_winner_for_line((i*3), (i*3)+1, (i*3)+2)
            if(winner != "")
                return winner;
        }

        // Diagonals
        winner = get_winner_for_line(0, 4, 8)
        if(winner != "")
            return winner
        winner = get_winner_for_line(2, 4, 6)
        if(winner != "")
            return winner

        return winner
    }

    def get_winner_for_line(i: Integer, j: Integer, k: Integer): String = {
        if(moves.contains(i) && 
            moves.contains(j) &&
            moves.contains(k) &&
            moves(i) == moves(j) && moves(i) == moves(k))
            return moves(i)
        return ""
    }

    def user_interact(step: Integer): Int = {
        println("Step " + step + ", Choose a position between A1 and C9:")
        var choice = ""
        while(!choices.contains(choice) || moves.contains(choices(choice))) {
            if(choice != "") {
                println("Choose between "+choices.keys.mkString(", "))
            }
            choice = Console.readLine()

            if(choice != null) {
                choice = choice.trim
            }
        }

        return choices(choice)
    }

    def ia_choice: Int = {
        val choices = ListBuffer[Integer]()
        for(i <- 0 to 8) {
            if(!moves.contains(i))
                choices += i
        }
        return choices(rand.nextInt(choices.size))
    }

    def draw {
        println("  A   B   C  ")
        println("- - - - - - -")
        for(i <- 0 to 2) {
            print("|")
            for(j <- 0 to 2) {
                var token = " "
                if(moves.contains((i*3)+j))
                    token = moves((i*3)+j)
                print(" "+token+" |")
            }
            println(" " + (i+1))
            println("- - - - - - -")
        }
        println
    }
}

Board.play
