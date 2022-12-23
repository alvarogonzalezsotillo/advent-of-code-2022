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

type Numero = Int
def log(s: => String) = println(s)



trait Node{
  def nodeNeigbours(v:Volcano) : Seq[Node]
  val name : String
  val isValve = false
  def asValve : Valve = ???
  def chamber : Chamber = ???
  val flow : Int
  override def toString = name
}


case class Valve( override val chamber: Chamber ) extends Node{
  override def nodeNeigbours(v:Volcano) = Seq(chamber)
  override val name = chamber.name + " valve"
  override val isValve = true
  override val asValve = this
  val flow = chamber.flow
}

class Chamber(override val name:String, val flow:Numero, val neigbours: Seq[String]) extends Node{
  override val chamber : Chamber = this
  
  //override val toString = s"Chamber $name to=${neigbours.mkString(",")}"
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
  val _distances : MMap[(Chamber,Chamber),Int] = MMap()
  val chamber = chambersSeq.map( c => (c.name, c) ).toMap
  val valves = chambersSeq.map(_.valve).filter(_ != null).toSet
  def distance( from: Node, to: Node ) = {
    val key = (from.chamber,to.chamber)
    _distances.get(key) match{
      case Some(d) => d
      case None => {
        val distances : MMap[Chamber,Int] = MMap()
        val toVisit : SortedSet[Chamber] = SortedSet()( new Ordering[Chamber]{
          def compare( n1: Chamber, n2: Chamber ) = distances.getOrElse(n1,0) - distances.getOrElse(n2,0) match{
            case 0 => n1.hashCode - n2.hashCode
            case n => n
          }
        })

        toVisit += from.chamber
        distances(from.chamber) = 0

        while( !toVisit.isEmpty && distances.get(to.chamber).isEmpty ){
          val node = toVisit.firstKey
          
          toVisit -= node
          val d = distances(node)
          for( n <- node.chamber.nodeNeigbours(this).filter(!_.isValve) ){
            if( distances.get(n.chamber).isEmpty ){
              distances(n.chamber) = d+1
              toVisit += n.chamber
            }
          }
        }
        val ret = distances(to.chamber)
        _distances(key) = ret
        _distances( (key._2, key._1) ) = ret
        ret
      }
    }
  }
}

var  currentMaxFlow:Numero=0


def dfs_memoized(volcano: Volcano, start: Node, time:Int ) : (Int,Seq[Node],Seq[Node]) = {
  case class Key( node: Node, elephant: Node, remainingTime:Int, openedValves: Set[Valve] )
  val memo = MMap[Key,(Int,List[Node],List[Node])]()

  var currentMax = 0

  def possibleFutureFlow_impl( node:Node, elephant: Node, remainingTime:Int, openedValves: Set[Valve]) = {
  }

  def dfs_memoized_impl( node: Node, elephant: Node, remainingTime:Int, openedValves: Set[Valve], visitedSinceValveOpened: Set[Node], visitedSinceValveOpenedEle: Set[Node], flowAccum:Int, flowPerMinute: Int ) : (Int,List[Node],List[Node]) = {

    if( remainingTime <= 0 ){
      return (0,List(node),List(elephant))
    }

    val key = Key(node,elephant,remainingTime,openedValves)

    assert( node != elephant || !node.isValve || !elephant.isValve )

    val closedValves = (volcano.valves -- openedValves)
    val (nearToNode,nearToElphant) = closedValves.partition( n => volcano.distance(n,node) <= volcano.distance(n,elephant) )

    val possibleFutureFlow = nearToNode.map( n => (1+remainingTime - volcano.distance(n,node))  * n.flow ).filter( _ > 0).sum +
        nearToElphant.map( n => (1+remainingTime - volcano.distance(n,node))  * n.flow ).filter( _ > 0).sum

    if( possibleFutureFlow + flowAccum + flowPerMinute*remainingTime < currentMax ){
      println( "No se puede superar el currentMax")
      println( s"  node:$node elephant:$elephant")
      println( s"  openedValves: $openedValves")
      println( s"  closedValves: $closedValves")
      println( s"  nearToNode:$nearToNode")
      println( s"  nearToElphant:$nearToElphant")
      println( s"  currentMax:$currentMax")
      println( s"  possibleFutureFlow:$possibleFutureFlow")
      println( s"  flowAccum:$flowAccum")
      println( s"  flowPerMinute:$flowPerMinute")
      println( s"  remainingTime:$remainingTime")
      return (0,List(),List())
    }

    memo.get( key ) match{
      case Some(ret) => {
        //if( log ) println( s"$indent cached")
        ret
      }

      case Some(_) | None => {
        val compute = {

          val nextTime =              remainingTime-1

          // PERSON
          val nextOpenedValves =      if(node.isValve) openedValves+node.asValve     else openedValves
          val thisFlow =              if(node.isValve) node.flow * (remainingTime-1) else 0
          val currentNode =           if(node.isValve) node.asValve.chamber          else node
          val nextVisitedSinceValve = if(node.isValve) Set(currentNode)              else visitedSinceValveOpened + node

          // ELEPHANT
          val nextOpenedValvesEle =      if(elephant.isValve) nextOpenedValves+elephant.asValve else nextOpenedValves
          val thisFlowEle =              if(elephant.isValve) elephant.flow * (remainingTime-1) else 0
          val currentNodeEle =           if(elephant.isValve) elephant.asValve.chamber          else elephant
          val nextVisitedSinceValveEle = if(elephant.isValve) Set(currentNodeEle)               else visitedSinceValveOpenedEle + elephant

          def visitable(n:Node) = !nextVisitedSinceValve.contains(n) && ( !n.isValve || !nextOpenedValvesEle.contains(n.asValve) )
          def visitableEle(n:Node) = !nextVisitedSinceValveEle.contains(n) && ( !n.isValve || !nextOpenedValvesEle.contains(n.asValve) )

          val search = for(
            n <- currentNode.nodeNeigbours(volcano)  if visitable(n) ;
            nEle <- currentNodeEle.nodeNeigbours(volcano)  if visitableEle(nEle) && ( !n.isValve || !nEle.isValve || n != nEle )
          ) yield{
            dfs_memoized_impl(n, nEle, nextTime, nextOpenedValvesEle, nextVisitedSinceValve, nextVisitedSinceValveEle, flowAccum+flowPerMinute, flowPerMinute+thisFlow+thisFlowEle )
          }

          val ret = if( search.isEmpty ){
            (0, List(),List())
          }
          else{
            val max = search.maxBy(_._1)
            (max._1+thisFlow+thisFlowEle, /*node ::*/ max._2 , /*elephant :: */ max._3 )
          }

          ret
        }
        memo(key) = compute

        if( compute._1 > currentMax ){
          currentMax = compute._1
          println( s"currentMax: $currentMax")
        }

        compute
      }
        
    }
  }

  dfs_memoized_impl(start,start, time+1,Set(),Set(),Set(),0,0)
}


val lines = LineIterator.lineIterator( new FileInputStream("input") )
val chambers = lines.map( Chamber.fromLine ).toList
val volcano = new Volcano(chambers)

for( c1 <- chambers ; c2 <- chambers ){
  //println( s"---- $c1 a $c2: ${volcano.distance(c1,c2)}")
}


val solution = LineIterator.time("solution 1"){
  dfs_memoized(volcano, volcano.chamber("AA"), 26)
}
println(s"Solution 1: ${solution._1}\n${solution._2}\n${solution._3}")
