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

    def play(players: Integer) {
        var winner = ""
        var step = 1

        draw

        while(winner == "" && step < 6) {
            moves(user_interact(step, "1st player")) = "X"

            if(step < 5) {
                if(players > 1) {
                    draw
                    moves(user_interact(step, "2nd player")) = "O"
                } else {
                    moves(ia_choice) = "O"
                }
            }

            draw

            winner = get_winner
            step += 1
        }

        if(players == 1) {
            if(winner == "X")
                println("You win !!")
            else {
                println("You failed !!")
            }
        } else {
            if(winner == "X") {
                println("1st player win !!")
            } else if(winner == "O") {
                println("2nd player win !!")
            } else {
                println("Nobody win.")
            }
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

    def user_interact(step: Integer, playerName: String): Int = {
        println("(step " + step + ") >> " + playerName + ", please choose a position between A1 and C9:")
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

var nPlayers = 1;
if(args.contains("--two-players")) {
    nPlayers = 2;
}

Board.play(nPlayers)
