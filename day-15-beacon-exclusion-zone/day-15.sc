// -*- mode: scala;coding:utf-8 -*-

import java.io._
import scala.util._
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue
import scala.collection.mutable.{Set => MSet,Map => MMap}
import $file.^.LineIterator

import collection.JavaConverters._

type Numero = Long
def log(s: => String) = println(s)


case class Location(x:Numero,y:Numero){
  def -(l:Location) = Location(x-l.x, y-l.y)
  def +(l:Location) = Location(x+l.x, y+l.y)
}

case class Range(minv: Numero, maxv:Numero){
  def iterator = (minv to maxv)
  def overlap(r:Range) = (minv max r.minv, maxv min r.maxv) match{
    case (mi,ma) if( mi <= ma ) =>  Some(Range(mi,ma))
    case _ => None
  }
  def union(r:Range) = overlap(r) match{
    case Some(_) => Some(Range( minv min r.minv, maxv max r.maxv))
    case None => None
  }
  def contains(i:Numero) = i >= minv && i <= maxv
  val size = maxv - minv + 1
}

class Ranges{
  val ranges = MSet[Range]()

  def contains(i:Numero) = ranges.exists( _.contains(i) )
  def size = ranges.map(_.size).sum

  def addRange(r:Range) = {
    val overlaps : Set[(Range,Range)]= ranges.
      zip( ranges ).
      map( p=> (p._1, r.overlap(p._1) ) ).
      filter( _._2.isDefined ).
      map( pair => (pair._1,pair._2.get)).
      toSet

    if( overlaps.size == 0 ){
      ranges += r
    }
    else{
      ranges --= overlaps.map(_._1)
      val toMerge = overlaps.map(_._1) + r
      val union = Range( toMerge.map(_.minv).min, toMerge.map(_.maxv).max )
      ranges += union
    }
  }

  def iterator : Iterator[Numero] = ranges.toList.sortBy(_.minv).map( r => (r.minv to r.maxv).iterator ).reduce( _ ++ _ )

  def intersect(r:Range) : Ranges = {
    val newRanges = ranges.map(_.overlap(r)).filter(_.isDefined).map(_.get)
    val ret = new Ranges()
    ret.ranges ++= newRanges
    ret
  }
}

case class Sensor(location:Location, nearestBeacon:Location){
  val distance = {
    val delta = (location-nearestBeacon)
    delta.x.abs + delta.y.abs
  }

  def rangeAtY(y:Numero) = {
    val x = location.x
    distance - (location.y - y).abs match{
      case d if d < 0 => None
      case d => Some(Range(x-d,x+d))
    }
  }
}

def parseLine(line:String)={
  val regex = s".*x=(-?[0-9]+).*y=(-?[0-9]+).*x=(-?[0-9]+).*y=(-?[0-9]+).*".r
  line match{
    case regex(sx,sy,bx,by) => Sensor( Location(sx.toInt,sy.toInt), Location(bx.toInt,by.toInt) )
  }
}

//val data = ("sample",10,Range(0,20))
val data =  ("input",2000000,Range(0,4000000)) 

val lines = LineIterator.lineIterator( new FileInputStream(data._1) )
val sensors = lines.filter(_.trim != "").map(parseLine).toList
val beacons = sensors.map(_.nearestBeacon).toSet


def coveredRange(y:Numero) = {
  val ranges = new Ranges()
  for( ro <- sensors.map(_.rangeAtY(y) ) ; r <- ro ){
    ranges.addRange(r)
  }
  ranges
}

val solution1 = {
  val y = data._2
  val ranges = coveredRange(y)
  ranges.size - beacons.filter( b => b.y == y && ranges.contains(b.x) ).size
}

println( "Solution 1: " + solution1 ) // 4737567

val span = data._3

// ⚠ NO ES SATISFACTORIO: tarda demasiado tiempo (> 5 minutos)
for( y <- span.iterator.filter( _ >= 2686239) ){
  val ranges = coveredRange(y).intersect(span)
  if( y % 1000 == 0 ){
    println( s"y: $y ranges: ${coveredRange(y).ranges}" )
  }
  if( ranges.size != span.size ){
    println( "Esta en y: " + y ) // 2686239
    val x = span.iterator.find( x => !ranges.contains(x) ).get
    println( "ESta en x:" + x ) // 3316868
    println( "Solution 2:" + (x*4000000+y) ) // 13267474686239

    System.exit(0)
  }
}


