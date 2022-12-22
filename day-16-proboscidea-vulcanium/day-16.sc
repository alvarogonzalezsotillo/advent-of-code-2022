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
def log(s: => String) = println(s)



trait Node{
  def nodeNeigbours(v:Volcano) : Seq[Node]
  val name : String
  val isValve = false
  def asValve : Valve = ???
  def asChamber : Chamber = ???
  val flow : Int
}


case class Valve( chamber: Chamber ) extends Node{
  override def nodeNeigbours(v:Volcano) = Seq(chamber)
  override val name = chamber.name + " valve"
  override val isValve = true
  override val asValve = this
  val flow = chamber.flow
}

class Chamber(override val name:String, val flow:Numero, val neigbours: Seq[String]) extends Node{
  override def asChamber : Chamber = this
  
  override val toString = s"Chamber $name to=${neigbours.mkString(",")}"
  val valve = if( flow > 0 ) Valve(this) else null

  private var _nodeNeigbours : Seq[Node] = null

  override def nodeNeigbours(v:Volcano) : Seq[Node] = {
    if( _nodeNeigbours != null ){
      _nodeNeigbours
    }
    else{
      _nodeNeigbours = {
        val ret = neigbours.map( v.chamber )
        if( valve != null ) ret :+ valve else ret
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
  val valves = chambersSeq.map(_.valve).filter(_ != null).toSet
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
    // if( currentFlow > currentMaxFlow ){
    //   currentMaxFlow = currentFlow
    //   println( s"flow:$currentMaxFlow on:${start.name} Valves: ${openedValves.map(_.name)} visited:${visited.map(_.name)}")
    // }
    
    return currentFlow
  }

  assert( !start.isInstanceOf[Valve] || !openedValves.contains(start.asInstanceOf[Valve]) )

  if( start.isValve ){
    val v = start.asValve
    // ABRO LA VALVULA Y VUELVO A LA CAMARA
    return dfs(volcano, v.chamber, remainingTime, openedValves + v, currentFlow, flowIncrement+v.chamber.flow, Set(), visited /* :+ v */ )
  }
  else{
    val c = start.asChamber
    val nextFlow = currentFlow + flowIncrement

    val nextVisitedValve = visitedSinceValveOpened + c
    val nextVisited = visited /* :+ c */
    val nextTime = remainingTime-1

    var ret = currentFlow

    for( n <- c.nodeNeigbours(volcano) if !visitedSinceValveOpened.contains(n) && ( !n.isValve || !openedValves.contains(n.asValve) ) ){
      ret = dfs(volcano, n, nextTime, openedValves, nextFlow, flowIncrement, nextVisitedValve, nextVisited) max ret
    }

    return ret
  }
}

def dfs_memoized(volcano: Volcano,start: Node, time:Int ) : (Int,Seq[Node]) = {
  case class Key( node: Node, remainingTime:Int, openedValves: Set[Valve] )
  val memo = MMap[Key,(Int,Seq[Node])]()

  def dfs_memoized_impl( node: Node, remainingTime:Int, openedValves: Set[Valve], visitedSinceValveOpened: Set[Node], visited: Seq[Node] ) : (Int,Seq[Node]) = {

    if( remainingTime <= 0 ){
      return (0,visited)
    }

    val key = Key(node,remainingTime,openedValves)
    lazy val indent = "   |" * (30-remainingTime)
    val log = false
    
    if( log ){
      println( s"$indent")
      println( s"$indent node:${node.name} remainingTime:$remainingTime")
      println( s"$indent openedValves:$openedValves")
      println( s"$indent visitedSinceValveOpened:$visitedSinceValveOpened")
      println( s"$indent visited:${visited.map(_.name)}")
    }
    

    memo.get( key ) match{
      case Some(ret) => {
        if( log ) println( s"$indent cached")
        ret
      }
      case None => {
        val ret = {

          val nextVisited =           visited //:+ node
          val nextOpenedValves =      if(node.isValve) openedValves+node.asValve     else openedValves
          val thisFlow =              if(node.isValve) node.flow * (remainingTime-1) else 0
          val currentNode =           if(node.isValve) node.asValve.chamber          else node
          val nextVisitedSinceValve = if(node.isValve) Set(currentNode)              else visitedSinceValveOpened + node
          val nextTime =              remainingTime-1


          
          def visitable(n:Node) = !nextVisitedSinceValve.contains(n) && ( !n.isValve || !nextOpenedValves.contains(n.asValve) )

          val search = for( n <- currentNode.nodeNeigbours(volcano) if visitable(n) ) yield{
            dfs_memoized_impl(n, nextTime, nextOpenedValves, nextVisitedSinceValve, nextVisited )
          }

          val ret = if( search.isEmpty ){
            (thisFlow, nextVisited)
          }
          else{
            val ret = search.maxBy(_._1)
            (ret._1+thisFlow,ret._2)
          }

          if( log ){
            println( s"$indent ret:${ret._1} ${ret._2.map(_.name)}")
          }

          ret
          /*
          if( node.isValve ){
            val v = node.asValve
            // ABRO LA VALVULA Y VUELVO A LA CAMARA
            val nextOpenedValves = openedValves+v
            val partialRet = dfs_memoized_impl(v.chamber, remainingTime, nextOpenedValves, Set())
            partialRet + v.chamber.flow * remainingTime
          }
          else{
            val c = node.asChamber
            val nextVisitedValve = visitedSinceValveOpened + c
            val nextTime = remainingTime-1

            def visitable(n:Node) = !visitedSinceValveOpened.contains(n) && ( !n.isValve || !openedValves.contains(n.asValve) )

            val search = for( n <- c.nodeNeigbours(volcano) if visitable(n) ) yield{
              dfs_memoized_impl(n, nextTime, openedValves, nextVisitedValve)
            }

            search.maxOption.getOrElse(0)
           }
          */
        }
        memo(key) = ret
        ret
      }
    }
  }

  dfs_memoized_impl(start,time,Set(),Set(),List())
}


val lines = LineIterator.lineIterator( new FileInputStream("sample") )
val chambers = lines.map( Chamber.fromLine ).toList
val volcano = new Volcano(chambers)

val solution = LineIterator.time("solution 1"){
  // dfs(volcano, volcano.chamber("AA"), 20)
  // 20 -> 1050 4s
  // 22 -> 1255 32s
  // 30 -> 2320 4444s

  dfs_memoized(volcano, volcano.chamber("AA"), 31)
  // 20 -> 1050 489 ms
  // 22 -> 1255 32s ms
  // 30 -> 2320 6s
}
println(s"Solution 1: ${solution._1} ${solution._2.map(_.name)}")
