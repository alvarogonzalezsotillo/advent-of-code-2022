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

case class Location(x:Numero,y:Numero){
  def -(l:Location) = Location(x-lx, y-ly)
  def components = (x,y) match{
    case (0,n) => Iterator.fill(n)(Location(0,1))
    case (n,0) => Iterator.fill(n)(Location(1,0))
    case _ => ???  
  }
}

case class Cave( minx: Int, miny: Int, maxx: Int, maxy: Int ){

  val array = Array.fill(maxx-minx+1,maxy-miny+1)(".")

  def inRange(x:Int,y:Int) = x >= minx && x <= maxx && y >= miny && x <= maxy

  def apply(x:Int,y:Int) = {
    if( inRange(x,y) ) array(x-minx,y-miny) else ".'"
  }

  def addPath( path: Seq[Location] )
}

type Path = IndexedSeq[Location]

def toPath(line: String) : Path = {
  line.
    split("->").
    map( _.split(",").map( s => {println("s:" + s );s.trim.toInt} ) ).
    map( a => Location(a(0),a(1)))
}

val lines = LineIterator.lineIterator( new FileInputStream("sample") )
val paths = lines.filter(_.trim != "").map(toPath).toIndexedSeq


println( paths.mkString("\n"))

val minx = paths.flatten.map(_.x)min
val maxx = paths.flatten.map(_.x)max
val miny = paths.flatten.map(_.y)min
val maxy = paths.flatten.map(_.y)max


