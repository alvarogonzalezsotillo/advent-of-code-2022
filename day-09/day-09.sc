// -*- mode: scala;coding:utf-8 -*-

import java.io._
import scala.util._
import scala.collection.mutable.{Set => MSet}
import $file.^.LineIterator


def log(s : => String) = println(s)

def signum(i:Int) : Int = Math.signum(i).toInt
def abs(i:Int) = Math.abs(i).toInt

case class Location(x: Int, y: Int){
  def -(l:Location) = this + l*(-1)
  def *(i: Int) = Location(x*i,y*i)
  def +(l:Location) = Location(x + l.x, y + l.y)
  def norm : Location = Location( signum(x), signum(y) )
  def components = Seq( Location(x,0).norm, Location(0,y).norm )
  def asSeq = Seq(x,y)
  def nextTo(l:Location) = (this-l).asSeq.forall(c=>abs(c)<=1)
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


def dump(){
  val xmin = visited.map(_.x).min min head.x
  val xmax = visited.map(_.x).max max head.x
  val ymin = visited.map(_.y).min min head.y
  val ymax = visited.map(_.y).max max head.y


  println()
  for( y <- 0 to ymax-ymin ){
    for( x <- 0 to xmax-xmin ){
      val ry = y+ymin
      val rx = x+xmin
      val l = Location(rx,ry)
      if( l == head ){
        print("H")
      }
      else if( l == tail ){
        print("T")
      }
      else if( visited.contains(l) ){
        print("#")
      }
      else{
        print(".")
      }
    }
    println()
  }
  println()
}


for( m <- movements){
  head = head + m
  //println( s"Head: $head  Tail:$tail nexto:${head.nextTo(tail)}")
  //println( s"  Delta: $delta")
  if( !head.nextTo(tail) ){
    val delta = head - tail
    delta.components.foreach{ c =>
      //println( s"  c: $c")
      if( tail + c != head ){
        tail = tail + c
        //println( s"  tail: $tail")
      }
    }
  }
  visited += tail
  //dump()
}

println( visited )
println( s"Solution: ${visited.size}")

assert( Location(1,1) == Location(1,1) )

/*
     _                         _                
 ___(_)_ __     __ _  ___ __ _| |__   __ _ _ __ 
/ __| | '_ \   / _` |/ __/ _` | '_ \ / _` | '__|
\__ \ | | | | | (_| | (_| (_| | |_) | (_| | |   
|___/_|_| |_|  \__,_|\___\__,_|_.__/ \__,_|_|   
                                                

 */
