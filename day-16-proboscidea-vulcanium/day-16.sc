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
  def asChamber : Chamber = ???
  def index : Int
}


case class Valve( chamber: Chamber ) extends Node{
  override def nodeNeigbours(v:Volcano) = Seq(chamber)
  override val name = chamber.name + " valve"
  override val isValve = true
  override val asValve = this
  override val index = chamber.index
}

class Chamber(override val name:String, val flow:Numero, val neigbours: Seq[String]) extends Node{
  override def asChamber : Chamber = this
  
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
  openedValves: Array[Array[Boolean]],
  visitedSinceValveOpened: Array[Array[Boolean]],
  currentFlow:Numero = 0,
  flowIncrement:Numero=0,
  visited: Seq[Node] = List()
) : Numero = {

  if( remainingTime <= 0 ){
    return currentFlow
  }

  assert( !start.isInstanceOf[Valve] || !inOpenedValves(start.asInstanceOf[Valve]) )

  def inVisitedSinceValveOpened(n:Node) = visitedSinceValveOpened(remainingTime)(n.index)
  def inOpenedValves(v:Valve) = openedValves(remainingTime)(v.index)

  if( start.isValve ){
    val v = start.asValve
    // ABRO LA VALVULA Y VUELVO A LA CAMARA
    openedValves(remainingTime)(v.index) = true
    val old = visitedSinceValveOpened(remainingTime)
    visitedSinceValveOpened(remainingTime) = new Array(visitedSinceValveOpened(remainingTime).size)
    val ret = dfs(volcano, v.chamber, remainingTime, openedValves,visitedSinceValveOpened, currentFlow, flowIncrement+v.chamber.flow, visited /* :+ v */ )
    visitedSinceValveOpened(remainingTime) = old
    return ret
  }
  else{
    val c = start.asChamber
    val nextFlow = currentFlow + flowIncrement

    /*
     if( nextFlow > currentMaxFlow ){
     println( s"flow:$nextFlow on:${start.name} Valves: ${openedValves.map(_.name)} visited:${visited.map(_.name)}")
     currentMaxFlow = nextFlow
     }
     */


    val nextVisited = visited /* :+ c */
    val search = for( n <- c.nodeNeigbours(volcano).toList if !inVisitedSinceValveOpened(n) && ( !n.isValve || !inOpenedValves(n.asValve) ) ) yield {

      System.arraycopy( visitedSinceValveOpened(remainingTime), 0, visitedSinceValveOpened(remainingTime-1), 0, visitedSinceValveOpened(remainingTime).length)
      visitedSinceValveOpened(remainingTime-1)(n.index) = true
      System.arraycopy( openedValves(remainingTime), 0, openedValves(remainingTime-1), 0, openedValves(remainingTime).length)
      
      dfs(volcano, n, remainingTime-1, openedValves, visitedSinceValveOpened, nextFlow, flowIncrement, nextVisited)
    }
    if( search.isEmpty ){
      return currentFlow
    }
    else{
      return search.max
    }
  }

}


val lines = LineIterator.lineIterator( new FileInputStream("input") )
val chambers = lines.map( Chamber.fromLine ).toList
val volcano = new Volcano(chambers)

val solution = LineIterator.time("solution 1"){
  val depth = 20
  dfs(volcano, volcano.chamber("AA"), depth, Array.fill(depth+1,volcano.chamber.size)(false), Array.fill(depth+1,volcano.chamber.size)(false) )
  // 1050 para profundidad 20
}
println(s"Solution 1:${solution}")
