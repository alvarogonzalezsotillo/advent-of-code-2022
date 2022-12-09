// -*- mode: scala;coding:utf-8 -*-

import java.io._
import scala.util._
import scala.collection.mutable.{Set => MSet}
import $file.^.LineIterator


def log(s : => String) = println(s)

def signum(i:Int) : Int = Math.signum(i).toInt

case class Location(x: Int, y: Int){
  def -(l:Location) = this + l*(-1)
  def *(i: Int) = Location(x*i,y*i)
  def +(l:Location) = Location(x + l.x, y + l.y)
  def norm : Location = Location( signum(x), signum(y) )
  def components = Seq( Location(x,0).norm, Location(0,y).norm )
}


def parseMovements(it: Iterator[String]) : Iterator[Location]= {
  val L = Location.apply(_,_)
  val movement = "(L|R|U|D) ([0-9]*)".r
  val vector = Map( "L"-> L(-1,0), "R"-> L(1,0), "U"->L(0,-1), "D"->L(0,1) )

  it.flatMap{ case movement(dir,size) => Array.fill(size.toInt)( vector(dir) ) }
}

val lines = LineIterator.lineIterator( new FileInputStream("input") )
val movements = parseMovements(lines)


var head = Location(0,0)
var tail = Location(0,0)
val visited = MSet[Location]()
visited += tail

for( m <- movements){
  head = head + m
  println( s"Head: $head  Tail:$tail")
  val delta = head - tail
  println( s"  Delta: $delta")
  delta.components.foreach{ c =>
    println( s"  c: $c")
    if( tail + c != head ){
      tail = tail + c
      println( s"  tail: $tail")
    }
    visited += tail
  }
}

println( s"Solution: ${visited.size}")


/*
     _                         _                
 ___(_)_ __     __ _  ___ __ _| |__   __ _ _ __ 
/ __| | '_ \   / _` |/ __/ _` | '_ \ / _` | '__|
\__ \ | | | | | (_| | (_| (_| | |_) | (_| | |   
|___/_|_| |_|  \__,_|\___\__,_|_.__/ \__,_|_|   
                                                

 */
