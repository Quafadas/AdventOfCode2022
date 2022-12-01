//> using lib "co.fs2::fs2-core:3.4.0"
//> using lib "co.fs2::fs2-io:3.4.0"

import cats.effect.{IO, IOApp}
import fs2.io.file.{Files, Path}

object Problem1 extends IOApp.Simple:
    def run = 
        val data1 = Files[IO].readUtf8Lines(Path("data/problem1.txt"))
            .groupAdjacentBy(_.isEmpty()) // Finds empty rows
            .filter(_._1 != true) // filters empty rows
            .map(_._2) // leaves behind a stream of chunks that we have interest in           
            .map(aChunk => aChunk.map(_.toInt).toList.sum ) 
            .zipWithIndex            
            .fold[(Int, Long)]((0,0l)){(pair1, pair2) => if pair1._1 > pair2._1 then pair1 else pair2 }
            //.evalTap(IO.println(_))
            //

        val data2 = Files[IO].readUtf8Lines(Path("data/problem1.txt"))
            .groupAdjacentBy(_.isEmpty()) // Finds empty rows
            .filter(_._1 != true) // filters empty rows
            .map(_._2) // leaves behind a stream of chunks that we have interest in           
            .map(aChunk => aChunk.map(_.toInt).toList.sum )             
            .fold[List[Int]](List(0,0,0)){(list, newInt) => (list :+ newInt).sorted.tail  }
            .evalTap(IO.println(_))    
        
        data1.compile.toList.flatMap(in => IO.println(in)) >>
        data2.compile.toList.flatMap(in => IO.println(in.flatten.sum))
        


