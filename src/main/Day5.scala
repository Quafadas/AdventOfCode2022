
import cats.effect.{IO, IOApp}
import fs2.io.file.{Files, Path}

import scala.collection.mutable.Stack

case class Move(numberCrates:Int, from: Int, to: Int )

val queues = Seq[Stack[Char]](
    Stack('V','J','B', 'D'),
    Stack('F','D','R', 'W', 'B','V','P'),
    Stack('Q','W','C', 'D', 'L','F','G', 'R'),
    Stack('B','D','N', 'L', 'M','P','J', 'W'),
    Stack('Q','S','C', 'P', 'B','N','H'),
    Stack('G','N','S', 'B', 'D','R'),
    Stack('H','S','F', 'Q', 'M','P', 'B','Z'),
    Stack('F','L','W'),
    Stack('R','M','F','V','S'),        
)

object Day5_a extends IOApp.Simple:

/* Starting point

        [Q] [B]         [H]        
    [F] [W] [D] [Q]     [S]        
    [D] [C] [N] [S] [G] [F]        
    [R] [D] [L] [C] [N] [Q]     [R]
[V] [W] [L] [M] [P] [S] [M]     [M]
[J] [B] [F] [P] [B] [B] [P] [F] [F]
[B] [V] [G] [J] [N] [D] [B] [L] [V]
[D] [P] [R] [W] [H] [R] [Z] [W] [S]
 1   2   3   4   5   6   7   8   9 
*/


    println(queues)

    def run = 
        val data1 = 
            Files[IO].readUtf8Lines(Path("data/problem5.txt"))
            .map(str => 
                val splitted = str.split(" ")                
                Move(splitted(1).toInt, splitted(3).toInt, splitted(5).toInt)
            )
            .fold(queues){ (newQueues, in) =>
                //println(in) 
                val queuefrom = newQueues(in.from - 1)
                val queueTo = newQueues(in.to - 1)
                //println(queuefrom)
                //println(queueTo)
                for(idx <- 1 to in.numberCrates) {
                    
                    val takeOut = queuefrom.pop()
                    queueTo.push(takeOut)
                }
                //println(newQueues) 
                newQueues
            }
            .last
            .map(result => result.get.map(_.head).mkString(""))

        data1.compile.toList.flatMap(in => IO.println(in))

object Day5_b extends IOApp.Simple:


    def run = 
        val data1 = 
            Files[IO].readUtf8Lines(Path("data/problem5.txt"))
            .map(str => 
                val splitted = str.split(" ")                
                Move(splitted(1).toInt, splitted(3).toInt, splitted(5).toInt)
            )
            .fold(queues){ (newQueues, in) =>
                //println(in) 
                val queuefrom = newQueues(in.from - 1)
                val queueTo = newQueues(in.to - 1)
                //println(queuefrom)
                //println(queueTo)
                val allPopped = for(idx <- 1 to in.numberCrates) yield {                    
                    queuefrom.pop()                    
                }
                queueTo.pushAll(allPopped.reverse)
                newQueues
            }
            .last
            .map(result => result.get.map(_.head).mkString(""))

        data1.compile.toList.flatMap(in => IO.println(in))
