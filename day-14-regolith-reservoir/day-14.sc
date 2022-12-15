// -*- mode: scala;coding:utf-8 -*-

import java.io._
import scala.util._
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue
import scala.collection.mutable.{Set => MSet,Map => MMap}
import $file.^.LineIterator

import collection.JavaConverters._

type Numero = Int

def signum(i:Int)=Math.signum(i).toInt

case class Location(x:Numero,y:Numero){
  def -(l:Location) = Location(x-l.x, y-l.y)
  def +(l:Location) = Location(x+l.x, y+l.y)
  def components  = (x,y) match{
    case (0,n) => (0 to n by signum(n)).map( Location(0,_) )
    case (n,0) => (0 to n by signum(n)).map( Location(_,0) )
  }
}

case class Cave( paths: Seq[Path], dropLocation:Location ){
  val allLocations = paths.flatten :+ dropLocation
  val minx = allLocations.map(_.x).min
  val maxx = allLocations.map(_.x).max
  val miny = allLocations.map(_.y).min
  val maxy = allLocations.map(_.y).max
  val array = Array.fill(maxx-minx+1,maxy-miny+1)('.')

  def inRange(l:Location) : Boolean = inRange(l.x,l.y)
  def inRange(x:Int,y:Int) = x >= minx && x <= maxx && y >= miny && y <= maxy

  def apply(l:Location) : Char = apply(l.x,l.y)
  def apply(x:Int,y:Int) : Char = {
    if( inRange(x,y) ) array(x-minx)(y-miny) else '.'
  }

  def set(l:Location, c:Char) = {
    if( inRange(l) ) array(l.x-minx)(l.y-miny) = c else ()
  }

  def update(l:Location,c:Char) = set(l,c)

  def addPath( path: Seq[Location] ) = {
    path.
      zip(path.tail).
      foreach{ case (l1,l2) =>
        (l1-l2).components.foreach( l => set(l+l2,'#'))
      }
  }

  def dump()={
    for( y <- miny to maxy ){
      for( x <- minx to maxx ){
        print( apply(x,y) )
      }
      println()
    }
    println()
  }

  paths.foreach(addPath)

  def downFrom(l:Location = dropLocation) = {
    Seq(Location(0,1),Location(-1,1),Location(1,1)).
      map(_+l).
      find( apply(_) == '.' )
  }

  def computeFinalDrop(l:Location = dropLocation ) = {
    Iterator.iterate[Option[Location]](Some(l)){ current =>
      downFrom(current.get) match{
        case Some(down) => Some(down)
        case None => None
      }
    }.takeWhile(_.isDefined).map(_.get).takeWhile(_.y<=maxy+1).toList.last
  }
}

type Path = IndexedSeq[Location]

def toPath(line: String) : Path = {
  line.
    split("->").
    map( _.split(",").map( _.trim.toInt ) ).
    map( a => Location(a(0),a(1)))
}

val lines = LineIterator.lineIterator( new FileInputStream("input") )
val paths = lines.filter(_.trim != "").map(toPath).toIndexedSeq


val cave = Cave(paths, Location(500,0))


cave.dump()

val it = Iterator.iterate(cave.computeFinalDrop()){ l =>
  val next = cave.computeFinalDrop()
  cave(next) = 'o'
  next
}.takeWhile(_.y <= cave.maxy)

println( "Solution 1:" + (it.size-1) ) // 618




