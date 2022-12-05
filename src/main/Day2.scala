
import cats.effect.{IO, IOApp}
import fs2.io.file.{Files, Path}

enum Result(score: Int):
    val scoreRes = score
    case Win extends Result(6)
    case Draw extends Result(3)
    case Lose extends Result(0)

enum Player:
    case P1
    case P2

enum GameChoice(score: Int):    
    val scoreRes = score
    case Rock extends GameChoice(1)
    case Paper extends GameChoice(2)
    case Scissors extends GameChoice(3)  

object Problem2_B extends IOApp.Simple:
    case class RPSGame(player1: Char, player2: Char)
    case class RPSGameChoices(player1: GameChoice, player2: Result)
    val lookupP1 : Map[(Player, Char), GameChoice] = Map(
        (Player.P1, 'A') -> GameChoice.Rock,
        (Player.P1, 'B') -> GameChoice.Paper,
        (Player.P1, 'C') -> GameChoice.Scissors,
    )       

    val lookupP2 : Map[(Player, Char), Result] = Map(
        (Player.P2, 'X') -> Result.Lose,
        (Player.P2, 'Y') -> Result.Draw,
        (Player.P2, 'Z') -> Result.Win,
    )       


    extension (on: RPSGame)
        //IRL should make this exception proof
        def parseChoices = 
            val p1 = lookupP1((Player.P1, on.player1))
            val p2 = lookupP2((Player.P2, on.player2))
            RPSGameChoices(p1, p2)

    extension (on: RPSGameChoices)
        def scoreGame: Int =
            val p2Choice : GameChoice = on match 
                case RPSGameChoices( GameChoice.Rock, Result.Win ) => GameChoice.Paper
                case RPSGameChoices( GameChoice.Rock, Result.Draw ) => GameChoice.Rock
                case RPSGameChoices( GameChoice.Rock, Result.Lose ) => GameChoice.Scissors
                case RPSGameChoices( GameChoice.Paper, Result.Win ) => GameChoice.Scissors
                case RPSGameChoices( GameChoice.Paper, Result.Draw ) => GameChoice.Paper
                case RPSGameChoices( GameChoice.Paper, Result.Lose ) => GameChoice.Rock
                case RPSGameChoices( GameChoice.Scissors, Result.Win ) => GameChoice.Rock
                case RPSGameChoices( GameChoice.Scissors, Result.Draw ) => GameChoice.Scissors
                case RPSGameChoices( GameChoice.Scissors, Result.Lose ) => GameChoice.Paper
            on.player2.scoreRes + p2Choice.scoreRes

    def run =         
        val data1 = Files[IO].readUtf8Lines(Path("data/problem2.txt"))
            .map(in => 
                val choices = in.split(" ")
                val game = RPSGame(in.head, in.last)
                game.parseChoices.scoreGame
            )
            .fold(0)(_ + _)
            //.evalTap(println(_))
            data1.compile.toList.flatMap(in => IO.println(in)) 
        


object Problem2_A extends IOApp.Simple:
    case class RPSGame(player1: Char, player2: Char)
    case class RPSGameChoices(player1: GameChoice, player2: GameChoice)
    val lookup : Map[(Player, Char), GameChoice] = Map(
        (Player.P1, 'A') -> GameChoice.Rock,
        (Player.P1, 'B') -> GameChoice.Paper,
        (Player.P1, 'C') -> GameChoice.Scissors,
        (Player.P2, 'X') -> GameChoice.Rock,
        (Player.P2, 'Y') -> GameChoice.Paper,
        (Player.P2, 'Z') -> GameChoice.Scissors,
    )   

    extension (on: RPSGame)
        //IRL should make this exception proof
        def parseChoices = 
            val p1 = lookup((Player.P1, on.player1))
            val p2 = lookup((Player.P2, on.player2))
            RPSGameChoices(p1, p2)

    extension (on: RPSGameChoices)
        def scoreGame: Int =
            val gameResult : Result = on match 
                case RPSGameChoices( GameChoice.Rock, GameChoice.Scissors ) => Result.Lose
                case RPSGameChoices( GameChoice.Rock, GameChoice.Paper ) => Result.Win
                case RPSGameChoices( GameChoice.Rock, GameChoice.Rock ) => Result.Draw
                case RPSGameChoices( GameChoice.Paper, GameChoice.Scissors ) => Result.Win
                case RPSGameChoices( GameChoice.Paper, GameChoice.Paper ) => Result.Draw
                case RPSGameChoices( GameChoice.Paper, GameChoice.Rock ) => Result.Lose
                case RPSGameChoices( GameChoice.Scissors, GameChoice.Scissors ) => Result.Draw
                case RPSGameChoices( GameChoice.Scissors, GameChoice.Paper ) => Result.Lose
                case RPSGameChoices( GameChoice.Scissors, GameChoice.Rock ) => Result.Win           
            gameResult.scoreRes + on.player2.scoreRes
    def run = 
        println(lookup)
        val data1 = Files[IO].readUtf8Lines(Path("data/problem2.txt"))
            .map(in => 
                val choices = in.split(" ")
                val game = RPSGame(in.head, in.last)
                game.parseChoices.scoreGame
            )
            .fold(0)(_ + _)
        

        // val data2 = Files[IO].readUtf8Lines(Path("data/problem1.txt"))
        //     .groupAdjacentBy(_.isEmpty()) // Finds empty rows
        //     .filter(_._1 != true) // filters empty rows
        //     .map(_._2) // leaves behind a stream of chunks that we have interest in           
        //     .map(aChunk => aChunk.map(_.toInt).toList.sum )             
        //     .fold[List[Int]](List(0,0,0)){(list, newInt) => (list :+ newInt).sorted.tail  }
        //     .evalTap(IO.println(_))    
        
        data1.compile.toList.flatMap(in => IO.println(in)) 
        //data2.compile.toList.flatMap(in => IO.println(in.flatten.sum))
        


