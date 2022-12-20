// -*- mode: scala;coding:utf-8 -*-

import java.io._
import scala.util._
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue
import scala.collection.mutable.{Set => MSet,Map => MMap}
import $ivy.`org.scala-lang.modules:scala-parallel-collections_2.13:1.0.4`

import scala.collection.parallel.CollectionConverters._
import $file.^.LineIterator

import collection.JavaConverters._

type Numero = Long
def log(s: => String) = println(s)



trait Node{
  def nodeNeigbours(v:Volcano) : Seq[Node]
  val name : String
  val isValve = false
  def asValve : Valve = ???
}


case class Valve( chamber: Chamber ) extends Node{
  override def nodeNeigbours(v:Volcano) = Seq(chamber)
  override val name = chamber.name + " valve"
  override val isValve = true
  override val asValve = this
}

class Chamber(override val name:String, val flow:Numero, val neigbours: Seq[String]) extends Node{
  override val toString = s"Chamber $name flow=$flow to=${neigbours.mkString(",")}"
  val valve = if( flow > 0 ) Valve(this) else null

  var index : Int = -1

  private var _nodeNeigbours : Seq[Node] = null

  override def nodeNeigbours(v:Volcano) : Seq[Node] = {
    if( _nodeNeigbours != null ){
      _nodeNeigbours
    }
    else{
      _nodeNeigbours = {
        val ret = neigbours.map( v.chamber )
        if( valve != null ){
          ret :+ valve
        }
        else{
          ret
        }
      }
      _nodeNeigbours
    }
  }
}
  



object Chamber{
  val chamberRegex = "Valve (.*) has flow rate=(.*); .* valve(?:s?) (.*)".r
  def apply(name:String,flow:String,neigbours:String) = new Chamber(name, flow.toInt, neigbours.split(",").map(_.trim))
  def fromLine(line:String) = line match{
    case chamberRegex(name,flow,valves) => Chamber(name,flow,valves)
  }
}

def toDot(chambers:Seq[Chamber]) = {
  println("digraph{")
  chambers.foreach( c => println(s"""  ${c.name}[label="${c.name} ${c.flow}"]"""))
  for( c <- chambers ; to <- c.neigbours ){
    println(  s"  ${c.name} -> $to" )
  }
  println("}")
}

class Volcano( chambersSeq: Seq[Chamber] ){
  val chamber = chambersSeq.map( c => (c.name, c) ).toMap
  chambersSeq.zipWithIndex.foreach{ case (c,i) => c.index = i }
}

var  currentMaxFlow:Numero=0


def dfs(
  volcano: Volcano,
  start: Node,
  remainingTime: Int,
  openedValves: Set[Valve] = Set(),
  currentFlow:Numero = 0,
  flowIncrement:Numero=0,
  visitedSinceValveOpened: Set[Node] = Set(),
  visited: Seq[Node] = List()
) : Numero = {

  if( remainingTime <= 0 ){
    return currentFlow
  }

  assert( !start.isInstanceOf[Valve] || !inOpenedValves(start.asInstanceOf[Valve]) )

  def inVisitedSinceValveOpened(n:Node) = visitedSinceValveOpened.contains(n)
  def inOpenedValves(v:Valve) = openedValves.contains(v)

  start match{
    case v: Valve => {
      // ABRO LA VALVULA Y VUELVO A LA CAMARA
      return dfs(volcano, v.chamber, remainingTime, openedValves + v, currentFlow, flowIncrement+v.chamber.flow, Set(), visited /* :+ v */ )
    }
    case c: Chamber =>{
      val nextFlow = currentFlow + flowIncrement

      /*
      if( nextFlow > currentMaxFlow ){
        println( s"flow:$nextFlow on:${start.name} Valves: ${openedValves.map(_.name)} visited:${visited.map(_.name)}")
        currentMaxFlow = nextFlow
      }
      */
      val nextVisitedValve = visitedSinceValveOpened + c
      val nextVisited = visited /* :+ c */
      val search = for( n <- c.nodeNeigbours(volcano).toList if !inVisitedSinceValveOpened(n) && ( !n.isValve || !inOpenedValves(n.asValve) ) ) yield {
        dfs(volcano, n, remainingTime-1, openedValves, nextFlow, flowIncrement, nextVisitedValve, nextVisited)
      }
      if( search.isEmpty ){
        return currentFlow
      }
      else{
        return search.max
      }
    }
  }

}


val lines = LineIterator.lineIterator( new FileInputStream("input") )
val chambers = lines.map( Chamber.fromLine ).toList
val volcano = new Volcano(chambers)

val solution = LineIterator.time("solution 1"){
  dfs(volcano, volcano.chamber("AA"), 20)
}
println(s"Solution 1:${solution}")
