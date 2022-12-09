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
  def nextTo(l:Location) = abs(x-l.x) <= 1 && abs(y-l.y) <= 1
}


def parseMovements(it: Iterator[String]) : Iterator[Location]= {
  val L = Location.apply(_,_)
  val movement = "(L|R|U|D) ([0-9]*)".r
  val vector = Map( "L"-> L(-1,0), "R"-> L(1,0), "U"->L(0,-1), "D"->L(0,1) )

  it.flatMap{ case movement(dir,size) => Array.fill(size.toInt)( vector(dir) ) }
}


def follow( head: Location, tail: Location ) : Location = {
  var newTail = tail

  if( !head.nextTo(newTail) ){
    (head - newTail).components.foreach{ c =>
      if( newTail + c != head ){
        newTail = newTail + c
      }
    }
  }

  newTail
}


def dump(head:Location, tail:Array[Location], visited: MSet[Location] ){
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
      else if( tail.contains(l) ){
        val index = tail.indexOf(l)
        print(index+1)
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

def process(movements: Iterator[Location], numberOfKnots: Int ) = {
  var head = Location(0,0)
  var tail = Array.fill(numberOfKnots)( Location(0,0) )
  val visited = MSet[Location]()
  visited += tail.last

  for( m <- movements){
    head = head + m
    tail(0) = follow(head,tail(0))
    for( t <- 1 until tail.size ){
      tail(t) = follow(tail(t-1),tail(t))
    }
    visited += tail.last
    //dump(head, tail, visited )
  }

  visited.size
}

{
  val lines = LineIterator.lineIterator( new FileInputStream("input") )
  val movements = parseMovements(lines)
  println( "Solution 1:" + process( movements, 1 ) )// 5883
}
{
  val lines = LineIterator.lineIterator( new FileInputStream("input") )
  val movements = parseMovements(lines)
  println( "Solution 2:" + process( movements, 9 ) )// 2367
}

