// -*- mode: scala;coding:utf-8 -*-

import java.io._
import scala.util._
import scala.collection.mutable.{Set => MSet}
import $file.^.LineIterator

def log(s : => String) = println(s)


type Trees = Seq[Seq[Int]]

case class Location(x: Int, y: Int)

def readTrees(lines: Iterator[String]) : Trees = lines.map( _.map( _ - '0') ).toSeq
def rows(implicit trees: Trees) = trees.size
def columns(implicit trees: Trees ) = trees(0).size

def viewFromTop( x: Int )( implicit trees: Trees ) = trees.map( row => row(x) )
def viewFromBottom( x: Int )( implicit trees: Trees ) = trees.reverse.map( row => row(x) )
def viewFromLeft( y: Int )( implicit trees: Trees ) = trees(y)
def viewFromRigth( y: Int )( implicit trees: Trees ) = trees(y).reverse



def computeVisibles( implicit trees: Trees ) = {


  def visible_( i: Int, s: Seq[Int] ) : Boolean =   s.take(i).forall( _ < s(i) )


  def visible(location: Location )(implicit trees: Trees ) : Boolean = {
    visible_( location.y, viewFromTop(location.x) ) ||
    visible_( rows - location.y - 1, viewFromBottom(location.x) ) ||
    visible_( location.x, viewFromLeft(location.y) ) ||
    visible_( columns - location.x - 1, viewFromRigth(location.y) )
  }
  
  val accum = MSet[Location]()

  for( y <- (0 until trees.size) ; x <- (0 until trees(y).size) ){
    val location = Location(x,y)
    if( visible(location ) ){
      accum += location
    }
  }

  accum.toSet
}


{
  val lines = LineIterator.lineIterator( new FileInputStream("input") )
  implicit val trees = readTrees(lines)
  val visibles = computeVisibles
  println( s"Solution 1: ${visibles.size}")
}

def viewDistance( i: Int, s: Seq[Int] ) = {

  var ret = 0
  var end = false
  /*
  for( t <- i+1 until s.size if !end ){
    // HABRÍA QUE HACERLO CON UN WHILE, EL end NO PARA EL BUCLE
    if( s(t) < s(i) ){
      ret += 1
    }
    else if( s(t) >= s(i) ){
      ret += 1
      end = true
    }
  }
   */
  var t = i+1
  while ( t < s.size && !end ){
    if( s(t) < s(i) ){
      ret += 1
    }
    else if( s(t) >= s(i) ){
      ret += 1
      end = true
    }
    t += 1
  }
  ret
}

def viewDistance(location: Location)(implicit trees: Trees ) : Int = {
  val a = viewDistance( location.y, viewFromTop(location.x) )
  val b = viewDistance( rows - location.y - 1, viewFromBottom(location.x) )
  val c = viewDistance( location.x, viewFromLeft(location.y) )
  val d = viewDistance( columns - location.x - 1, viewFromRigth(location.y) )
  val ret = a*b*c*d

  //println(s"$location -- $a $b $c $d -- $ret")
  ret
}


{
  val lines = LineIterator.lineIterator( new FileInputStream("input") )
  implicit val trees = readTrees(lines)
  val solution = (for( x <- 0 until columns ; y <- 0 until rows ) yield Location(x,y)).map(viewDistance).max
  println( s"Solution 2: $solution")
}
