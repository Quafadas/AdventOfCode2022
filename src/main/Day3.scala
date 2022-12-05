
import cats.effect.{IO, IOApp}
import fs2.io.file.{Files, Path}



object Day3 extends IOApp.Simple:
    val littleScores = ('a' to 'z').zipWithIndex.map(in => (in._1, in._2+1))
    val bigScores = ('A' to 'Z').zipWithIndex.map(in => (in._1, in._2+27))
    val allPriorities = littleScores ++ bigScores

    def run = 
        val data1 = 
            Files[IO].readUtf8Lines(Path("data/problem3.txt"))
            .fold(0)(
                (total, str) => 
                    val len = str.length
                    val firstPart = str.take(len /2)
                    val secondPart = str.takeRight(len /2 )
                    val intersection = firstPart.toCharArray().intersect(secondPart.toCharArray())
                    
                    val value = allPriorities.find(_._1 == intersection.head).map(_._2).get
                    println(intersection.mkString(",") + s" : $value" )
                    (total + value)
            ) 
        data1.compile.toList.flatMap(in => IO.println(in))


object Day3_b extends IOApp.Simple:
    val littleScores = ('a' to 'z').zipWithIndex.map(in => (in._1, in._2+1))
    val bigScores = ('A' to 'Z').zipWithIndex.map(in => (in._1, in._2+27))
    val allPriorities = littleScores ++ bigScores

    def run = 
        val data1 = 
            Files[IO].readUtf8Lines(Path("data/problem3.txt"))
            .sliding(3, 3)
            .map( inChunk => 
                val common = inChunk.toList.foldLeft( allPriorities.map(_._1) )( (remaining, thisRucksack) => remaining.intersect(thisRucksack.toCharArray()) ).head
                allPriorities.find(_._1 == common).map(_._2).get
            )
            .fold(0)(_ + _)

        data1.compile.toList.flatMap(in => IO.println(in))