// -*- mode: scala;coding:utf-8 -*-

import java.io._
import scala.util._
import $file.^.LineIterator

def log(s : => String) = println(s)

case class Range(start:Int, end:Int){
  def intersection(r:Range) = {
    val s = start max r.start
    val e = end min r.end
    if( s <= e ) Some(Range(s,e)) else None
  }
  val size = end - start + 1
}

def fullyContained(r1: Range, r2: Range) = {
  r1.intersection(r2).
    map( r => Seq(r1.size,r2.size).contains( r.size ) ).
    getOrElse(false)
}

implicit class Tuple2ToArray[A]( t: (A,A) ){
  // NUNCA ME GUSTARON LAS TUPLAS 
  def apply(i:Int) = i match{
    case 0 => t._1
    case 1 => t._2
  }
}

val pairs = "(.*)-(.*),(.*)-(.*)".r

val lines = LineIterator.lineIterator( new FileInputStream("input") )
val ranges = lines.map( _ match{
  case pairs(s1,e1,s2,e2) => (Range(s1.toInt,e1.toInt), Range(s2.toInt,e2.toInt))
}).toSeq

val solution1 = ranges.filter( p => fullyContained(p(0), p(1)) ).size
println( s"Solution 1: $solution1") // 494

val solution2 = ranges.filter( p => p(0).intersection(p(1)).isDefined ).size
println( s"Solution 1: $solution2") // 833
