// -*- mode: scala;coding:utf-8 -*-

import java.io._
import scala.util._
import $file.^.LineIterator

def log(s : => String) = ()// println(s)


def tests() = {
  assert( solve("bvwbjplbgvbhsrlpgdmjqwftvncz" ) == 5 )
  assert( solve("nppdvjthqldpwncqszvftbrmjlhg") == 6 )
  assert( solve("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") == 10 )
  assert( solve("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") == 11 )
}



def solve(line:String, size: Int = 4) = {
  val solution = line.sliding(size).zipWithIndex.filter{ case (group,index) => group.toSet.size == size }.next
  val ret = solution._2 + size ;
  println(s"Solution: $solution  $ret")
  ret
}

val lines = LineIterator.lineIterator( new FileInputStream("input") )
val line = lines.next

{
  println( "SOLUTION 1")
  solve( line )
}

{
  println( "SOLUTION 2")
  solve( line, 14 )
}


