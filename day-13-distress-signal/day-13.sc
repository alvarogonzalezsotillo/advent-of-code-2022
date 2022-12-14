// -*- mode: scala;coding:utf-8 -*-

import java.io._
import scala.util._
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue
import scala.collection.mutable.{Set => MSet,Map => MMap}
import $file.^.LineIterator


import $ivy.`com.googlecode.json-simple:json-simple:1.1.1`
import org.json.simple.parser.JSONParser
import org.json.simple._

import collection.JavaConverters._

type Numero = Long
type AList = Seq[Any]

object parser{
  val jsonParser = new JSONParser()
  def toAList[T]( a: JSONArray ) : AList = {
    for( i <- a.iterator.asScala.toSeq ) yield i match{
      case a : JSONArray => toAList(a)
      case t : T => t
    }
  }
  def apply(s: String) = {
    val json = jsonParser.parse( s ).asInstanceOf[JSONArray]
    toAList(json)
  }
}

def compare( s1: AList, s2: AList ) : Int = {
  @tailrec
  def compare_( s1: AList, s2: AList ) : Int = {
    val ret = (s1.headOption, s2.headOption ) match{
      case (Some(i1:Numero), Some(i2:Numero)) => Math.signum(i1 - i2).toInt
      case (Some(se1:AList), Some(se2:AList)) => compare(se1,se2)
      case (Some(se:AList),  Some(i:Numero))  => compare(se,Seq(i))
      case (Some(i:Numero),  Some(se:AList))  => compare(Seq(i),se)
      case (Some(_),         None)            => 1
      case (None,            Some(_))         => -1
      case (None,            None)            => return 0 // ⚠️ WARNING, A RETURN
    }
    if( ret != 0 ) ret else compare_( s1.tail, s2.tail )
  }

  compare_(s1,s2)
}

val lines = LineIterator.lineIterator( new FileInputStream("input") )

val pairs = lines.dropWhile(_.trim == "").
  grouped(3).
  map( _.toSeq.take(2) ).
  map( _.map( parser(_) ) ).
  toList

val solution = pairs.map( p => compare(p(0),p(1)) ).
  zipWithIndex.filter( _._1 < 0 ).
  map( _._2+1 )

println( "Solution 1: " + solution.sum) // 5350

val packets = pairs.
  flatten(_.asInstanceOf[IterableOnce[Seq[AList]]])
  // ⚠️ Scala get lost with Any and type inference

val aditionalPackets = List( parser("[[2]]"), parser("[[6]]") )
val packetsSorted = (packets ++ aditionalPackets).sortWith( compare(_,_) < 0 )
val indexes = aditionalPackets.map( p => packetsSorted.indexOf(p)+1 )
println( "Solution 2: " + indexes.product ) // 19570
