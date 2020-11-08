
import scala.util.Random
import scala.collection.mutable.ListBuffer
import util.control.Breaks._


object perso {
    def main(args: Array[String]):Unit = {

        var board : List[String] = List("0","1","2","3","4","5","6","7","8","9")
        var list_check : List[String] = List()
        var res = "yes"

        def DrawBoard(board : List[String] ) : Unit = {
            println("   |   |")
            println(" "+ board(7)+" | "+ board(8)+ " | "+ board(9) )
            println("   |   |")
            println(" " + board(4) + " | " + board(5) + " | " + board(6))
            println("   |   |")
            println(" " + board(1) + " | " + board(2) + " | " + board(3))
        }
            
        def choose_symbol(): String  = {
            println("Do you want to be X or O ?")
            var letter = scala.io.StdIn.readLine()
            if (letter=="X" || letter=="0") {
            	return letter
                }
            else {
            	println("Please choose between X and O only")
            	choose_symbol()
            }
        }
 		def name_player1(): String  = {
            println("Player 1, what is your name ?")
            var name1 = scala.io.StdIn.readLine()
            return name1
        }
        def name_player2(): String  = {
            println("Player 2, what is your name ?")
            var name2 = scala.io.StdIn.readLine()
            return name2
        }

        def play_again(): String={
            println("Do you want to play again? (yes or no)?")
            res = scala.io.StdIn.readLine()
            if(res == "yes"){
            	list_check = List()
            	board = List("0","1","2","3","4","5","6","7","8","9")
            	println("The looser start the new game, good luck !")

            	DrawBoard(board)
            }
            else if(res == "no"){
            	println("Have a nice day!")
            	break
            }
            else{
            	println("Please choose between yes ou no")
            	play_again()
            }
            return res
        }


        def makeMove(board: List[String], letter: String, move: Int): List[String]={
            var board_up : List[String] =  board.updated(move,letter)
            return board_up
        }


        def IsWinner(b :List[String] ,l: String):Boolean={
            return b(7)==l && b(8)==l && b(9)==l | 
            b(4)==l && b(5)==l && b(6)==l |
            b(1)==l && b(2)==l && b(3)==l |
            b(7)==l && b(4)==l && b(1)==l |
            b(8)==l && b(5)==l && b(2)==l |
            b(9)==l && b(6)==l && b(3)==l |
            b(7)==l && b(5)==l && b(3)==l |
            b(9)==l && b(5)==l && b(1)==l 
            

        }
        def IsWin(board:List[String],l: String):Boolean={
            return IsWinner(board:List[String],l: String)

        }




        def player_move(board:List[String]): String = {
            println("What number do you want to play ? ")
            var num = scala.io.StdIn.readLine()

            val num_str =num.toString

            if (RihtKeys(num_str)== false){ 
                println("Pick a number between 1 and 9")
                player_move(board)
            }
            else if (MoveCheck(num_str)== true){ 
                println("This number has already been taken, please choose another one")
                //num = scala.io.StdIn.readLine()
                player_move(board)
            }
            else {
            	val num_int = num.toInt
            	return board(num_int)
            }
        }

        def RihtKeys(move:String): Boolean={
            val move_str =move.toString
            val keys : List[String] = List("1", "2","3", "4", "5", "6", "7", "8", "9")
            return  keys.contains(move_str)
        }

        def MoveCheck(move:String): Boolean={
        	val move_str =move.toString
        	val check = list_check.contains(move_str)
        	if(list_check.contains(move_str) == true){
            	list_check = list_check
        	}
        	else{
        		list_check = list_check:+ move_str
        	}
            return  check
        }

        def Player2Letter(letterPlayer1:String):String={
            if (letterPlayer1=="X") { 
                val  player2_letter="O"
                return player2_letter 
                }
            else { val player2_letter = "X" 
                return  player2_letter   
                }
        }

        def draw(board:List[String]):Boolean={
            return board(1) !="1" &&
            board(2) !="2" &&
            board(3) !="3" &&
            board(4) !="4" &&
            board(5) !="5" &&
            board(6) !="6" &&
            board(7) !="7" &&
            board(8) !="8" &&
            board(9) !="9" 

        }

        def Isdraw(board:List[String]):Boolean={
            return draw(board:List[String])

        }

        def Available_play(board:List[String]):List[String]={
            return board.filterNot(_.startsWith("X"))filterNot(_.startsWith("O"))filterNot(_.startsWith("0")) 
        }

        def getRandomElement(Liste_play: List[String]): String = {
            val random_var = new Random
            Liste_play(random_var.nextInt(Liste_play.length))
        }

       def computer_play(letter_player:String, available_playList:List[String] ):String={
               val random_elmt = getRandomElement(available_playList)
               return random_elmt
       }

       def max(a:Int,b:Int):Int={
           if (a>b) {return a}
           else if (b>a){return b}
           else (return a)
       }
        def min(a:Int,b:Int):Int={
           if (a>b) {return b}
           else if (b>a){return a}
           else (return a)
       }


        def minimax(board: List[String], maximizing:Boolean, player_letter: String, computer_letter: String): Int= {
            val available_playList= Available_play(board)
            if (IsWin(board, computer_letter) == true) { return 10 } // win
            else if (IsWin(board,player_letter) == true ) { return -10 } // loss
            else if (Isdraw(board) == true) { return 0 } // draw

            if (maximizing) {
                var bestEval = -9999

                for (move <-available_playList )  { // find the move with the highest evaluation
                    val result = minimax(makeMove(board, computer_letter, move.toInt), false, player_letter,computer_letter)
                    bestEval = max(result, bestEval)
                }
                return bestEval
            }
            
            else { // minimizing
                var worstEval = 9999
                for (move <- available_playList){
                    val result = minimax(makeMove(board, computer_letter, move.toInt), true, player_letter,computer_letter)
                    worstEval = min(result, worstEval)
                }
            return worstEval

        }
    } 


    def findBestMove(board: List[String], computer_letter:String):Int={
        val player_letter = Player2Letter(computer_letter)
        var bestEval = -9999
        var bestMove = -1
        val available_playList= Available_play(board)
        if (board(5) =="5") {return 5}
        for (move <- available_playList) {
            if(IsWin(makeMove(board, player_letter, move.toInt),player_letter)){return move.toInt}
            var result = minimax(makeMove(board, computer_letter, move.toInt),  false,player_letter, computer_letter)
            if (result > bestEval) {
                bestEval = result
                bestMove = move.toInt
            }
        }
        return bestMove
    }
    

  
    println("Welcome to Tic Tac Toc !")
    var name1 = name_player1()
    var name2 = name_player2()
    println(name1, "please choose your symbol")
    var letter = choose_symbol()
    println(name1, " you are : ",letter)
    //println("Player 2, is your turn")
    var letter_2 =Player2Letter(letter)
    println(name2, "you are : ", letter_2)
    DrawBoard(board)
    while(res == "yes"){
        println(name1, ", it's your turn")
        var move = player_move(board)  	
		board = makeMove(board, letter, move.toInt)
        DrawBoard(board)
        if(IsWinner(board, letter) == true){
            println("Congrats", name1, "You won")
            play_again()
            }
        if (Isdraw(board) == true ){
            println("Is a draw")
            play_again()
            }
        println(name2, "it's your turn")

        //Ligne pour que un joueur humain joue
        //var move2 = player_move(board)
        //ligne pour que un computeur joue un faux aléatoire :
        //var move2 = computer_play(letter_2, Available_play(board))
        //ligne pour algo min max
        var move2 = findBestMove(board,letter_2)
        MoveCheck(move2.toString)
        board = makeMove(board, letter_2, move2.toInt)
        DrawBoard(board)
        if (IsWinner(board, letter_2) == true ){
            println("Congrats" ,name2, "You won")
            play_again()
        }
         if (Isdraw(board) == true ){
            println("Is a draw")
            play_again()
            }
    }

}
}