// -*- mode: scala;coding:utf-8 -*-

import java.io._
import scala.util._
import $file.^.LineIterator


def log(s : => String) = () //println(s)


val lines = LineIterator.lineIterator( new FileInputStream("input") )

case class State(val currentMax: Seq[Long], val currentAccum: Long )

def toLong(s:String) = Try( java.lang.Long.parseLong(s) ).toOption

val result = lines.map(toLong).foldLeft( State(Seq(0,0,0),0) ){ case (State(m,a) , long) =>
  long match{
    case Some(l) => State(m,a+l)
    case None => {
      val array = (m :+ a).sorted.reverse.take(3)
      State( array, 0 )
    }
  }
}

println( result )
println( result.currentMax.sum )
