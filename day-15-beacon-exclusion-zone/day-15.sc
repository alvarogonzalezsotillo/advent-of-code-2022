// -*- mode: scala;coding:utf-8 -*-

import java.io._
import scala.util._
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue
import scala.collection.mutable.SortedSet
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
  def overlaps(r:Range) = (minv max r.minv) <= (maxv min r.maxv)
  def overlapNoCheck(r:Range) = Range(minv max r.minv, maxv min r.maxv)
  def unionNoCheck(r:Range) = Range(minv min r.minv, maxv max r.maxv)
  def contains(i:Numero) = i >= minv && i <= maxv
  val size = maxv - minv + 1
}

implicit val rangeOrdering = new Ordering[Range]{
    def compare(r1:Range,r2:Range) = (r1.minv - r2.minv).signum
}

class Ranges{

  val ranges = SortedSet[Range]()

  def contains(i:Numero) = ranges.exists( _.contains(i) )
  def size = ranges.map(_.size).sum

  val addRange = addRangeNonFunctional _
  val intersection = intersectionNonFunctional _

  def addRangeNonFunctional(range:Range) = {
    var current = range
    /*
    for( r <- ranges.iterator.toList ){
      if( r.overlaps(current) ){
        current = r.unionNoCheck(current)
        ranges -= r
      }
     }
     */
     ranges += current
     
  }

  def intersectionNonFunctional(r:Range,reusableRanges:Ranges=null) : Ranges = {
    val ret = if( reusableRanges != null ) reusableRanges else new Ranges()
    ret.ranges.clear
    val it = ranges.iterator.
      dropWhile( range => range.maxv < r.minv ).
      takeWhile( range => range.minv < r.maxv ) 

    for( range <- it ){
      ret.ranges += range.overlapNoCheck(r)
    }
    ret
  }

  def findFirstEmpyPosition(from:Numero, to:Numero) : Option[Numero] = {
    //println( s"findFirstEmpyPosition: from:$from to:$to")
    var i = from
    var ret : Option[Numero] = None
    for( r <- ranges if ret.isEmpty && i < to ){
      //println( s"findFirstEmpyPosition: r:$r i:$i")
      
      if( i < r.minv ){
        ret = Some(i)
        //println( s"findFirstEmpyPosition: found:$ret")
      }
      else{
        i = r.maxv + 1
        //println( s"findFirstEmpyPosition: avanzo i:$i")
      }
    }
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


def coveredRange(y:Numero, reusableRanges: Ranges = null ) = {
  val ranges = if( reusableRanges == null ) new Ranges else reusableRanges
  ranges.ranges.clear
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
val ini = System.currentTimeMillis
val reusableRanges1 = new Ranges()
val reusableRanges2 = new Ranges()
LineIterator.time("Parte 2"){
  var found = false
  for( y <- span.iterator/*.filter( _ >= 2686239)*/ if !found ){
    val ranges : Ranges = coveredRange(y,reusableRanges1)
    val emptyPos = ranges.findFirstEmpyPosition( span.minv, span.maxv )
    

    if( y % 10000 == 1 ){
      val now = System.currentTimeMillis
      println( s"y: $y ranges: ${coveredRange(y).ranges}" )
      println( s"time:${now-ini} ${(now.toDouble-ini)/y}")
    }
    if( emptyPos.isDefined ){
      println( "Esta en y: " + y ) // 2686239
      val x = span.iterator.find( x => !ranges.contains(x) ).get
      println( "ESta en x:" + x ) // 3316868
      println( "Solution 2:" + (x*4000000+y) ) // 13267474686239

      found = true
    }
  }
}

