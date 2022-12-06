
import cats.effect.{IO, IOApp}
import fs2.io.file.{Files, Path}

import scala.collection.mutable.Stack

object Day6_a extends IOApp.Simple:

    def run = 
        val data1 = 
            Files[IO].readUtf8(Path("data/problem6.txt"))
            .flatMap{in => fs2.Stream.emits(in.toCharArray())}
            .sliding(14)
            .zipWithIndex
            .evalTap(IO.println)
            .find((inStr, idx) =>                 
                val firstList = inStr.toList
                firstList.toSet.size == firstList.length
            )
            

        data1.compile.toList.flatMap(in => IO.println(in))