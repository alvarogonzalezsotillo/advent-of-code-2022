// -*- mode: scala;coding:utf-8 -*-

import java.io._
import scala.util._
import scala.collection.mutable.{Set => MSet}
import $file.^.LineIterator


def log(s : => String) = println(s)


def operations( in: Iterator[String] ) = {
  val noop = ".*noop.*".r
  val addx = ".*addx (-?[0-9]*).*".r
  in.flatMap{
    case noop() => Iterator(0)
    case addx(v) => Iterator(0,v.toInt)  
  }
}

def computeHistory( in: Seq[Int] ) = {
  var x = 1
  var nextX = x
  for( o <- in ) yield{
    x = nextX
    nextX += o
    x
  }
}

def strength( steps: Seq[Int], history: Seq[Int] ) = steps.map(s => s*history(s-1)).sum


val lines = LineIterator.lineIterator( new FileInputStream("input") )
val history = computeHistory(  operations(lines).toSeq )
println( "Solution 1: " + strength( Seq(20,60,100,140,180,220), history ) ) //15260

val screen = Array.fill(6,40)('?')
def draw( history: Seq[Int], screen: Array[Array[Char]] ){
  assert(history.size == screen.size*screen(0).size)
  for( s <- 0 until history.size; x = s%40; y = s/40 ){
    screen(y)(x) = if( Math.abs(x - history(s) ) < 2 ) '#' else '.'
  }
}

draw(history,screen)
println( screen.map( _.mkString).mkString("\n") ) // PGHFGLUG (o V en vez de U)

