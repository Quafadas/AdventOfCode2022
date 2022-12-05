
import cats.effect.{IO, IOApp}
import fs2.io.file.{Files, Path}



object Day4_a extends IOApp.Simple:
    val littleScores = ('a' to 'z').zipWithIndex.map(in => (in._1, in._2+1))
    val bigScores = ('A' to 'Z').zipWithIndex.map(in => (in._1, in._2+27))
    val allPriorities = littleScores ++ bigScores

    def run = 
        val data1 = 
            Files[IO].readUtf8Lines(Path("data/problem4.txt"))
            .map( inStr =>
                val asList = inStr.split(",")
                val elf1 = asList.head.split("-")
                val elf1Start = elf1.head.toInt
                val elf1Finish = elf1.last.toInt
                val elf2 = asList.last.split("-")
                val elf2Start = elf2.head.toInt
                val elf2Finish = elf2.last.toInt
                println(elf1Start)
                println(elf1Finish)
                val elf1Range = (elf1Start to elf1Finish).toSet
                val elf2Range = (elf2Start to elf2Finish).toSet
                // elf1Range.subsetOf(elf2Range) || elf2Range.subsetOf(elf1Range) // part1
                !elf1Range.intersect(elf2Range).isEmpty // part2

            )
            .collect{
                case true => 1
                case false => 0
            }.fold(0)(_ + _)
                        
        data1.compile.toList.flatMap(in => IO.println(in))


object Day4_b extends IOApp.Simple:

    def run = IO.println("")