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

  var index = -1

  private var _nodeNeigbours : Array[Node] = null

  override def nodeNeigbours(v:Volcano) : Seq[Node] = {
    if( _nodeNeigbours != null ){
      _nodeNeigbours
    }
    else{
      _nodeNeigbours = {
        val ret = neigbours.map( v.chamber )
        if( valve != null ) ret :+ valve else ret
      }.reverse.toArray

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
  val _distances = Array.fill(chambersSeq.size, chambersSeq.size)(-1)
  val chamber = chambersSeq.map( c => (c.name, c) ).toMap
  val valves = chambersSeq.map(_.valve).filter(_ != null).toSet

  chambersSeq.zipWithIndex.foreach{ case(c,i) => c.index = i }

  def distances_nocached(from: Chamber, to: Chamber ) = {
    val distances : MMap[Chamber,Int] = MMap()
    val toVisit : SortedSet[Chamber] = SortedSet()( new Ordering[Chamber]{
      def compare( n1: Chamber, n2: Chamber ) = distances.getOrElse(n1,0) - distances.getOrElse(n2,0) match{
        case 0 => n1.hashCode - n2.hashCode
        case n => n
      }
    })

    toVisit += from
    distances(from) = 0

    while( !toVisit.isEmpty && distances.get(to).isEmpty ){
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
    distances(to)
  }

  def distance( fromN: Node, toN: Node ) = {
    val from = fromN.chamber
    val to = toN.chamber
    _distances(from.index)(to.index) match{
      case d if d >= 0 => d
      case -1 => {
        val ret = distances_nocached(from,to)
        _distances(from.index)(to.index) = ret
        _distances(to.index)(from.index) = ret
        ret
      }
    }
  }
}

type Solution = Int // (Int,Seq[Node],Seq[Node])


def dfs_memoized(volcano: Volcano, start: Node, time:Int ) : Solution = {
  case class Key( node: Node, elephant: Node, remainingTime:Int, openedValves: Set[Valve] )
  val memo = MMap[Key,Solution]()

  var currentMax = 0

  case class State(node: Node, elephant: Node, remainingTime:Int, openedValves: Set[Valve], closedValves: Set[Valve], visitedSinceValveOpened: Set[Node], visitedSinceValveOpenedEle:Set[Node], flowAccum: Int, flowPerMinute: Int )

  def dfs_memoized_impl( s: State ) : Solution = {

    if( s.remainingTime <= 0 ){
      //return (0,List(node),List(elephant))
      return 0
    }

    val key = Key(s.node,s.elephant,s.remainingTime,s.openedValves)

    //assert( node != elephant || !node.isValve || !elephant.isValve )

    def saveKey(ret:Solution) = {
      memo(key) = ret
      ret
    }

    if( memo.isDefinedAt(key) ){
      memo(key)
    }
    else{

      if( s.closedValves.size == 0 ){
         val ret = 0
         saveKey(ret)
         //println( s"hit $ret $s")
         return ret
      }

      def computePossibleFutureFlow(n:Node) = ((s.remainingTime-1) - (volcano.distance(n,s.node) min volcano.distance(n,s.elephant))) * n.flow

      val possibleFutureFlow = s.closedValves.foldLeft(0){ (accum,v) => accum + (0 max computePossibleFutureFlow(v) ) }

      if( possibleFutureFlow + s.flowAccum + s.flowPerMinute * s.remainingTime < currentMax ){
        // println( "No se puede superar el currentMax")
        // println( s"  node:${s.node} elephant:${s.elephant}")
        // println( s"  openedValves: ${s.openedValves}")
        // println( s"  closedValves: ${s.closedValves}")
        // println( s"  currentMax:$currentMax")
        // println( s"  possibleFutureFlow:$possibleFutureFlow")
        // println( s"  flowAccum:${s.flowAccum}")
        // println( s"  flowPerMinute:${s.flowPerMinute}")
        // println( s"  remainingTime:${s.remainingTime}")
        // saveKey( (0,List(),List()) )

        return  saveKey(0)
      }
      
      val compute = {

        val nextTime =              s.remainingTime-1

        // PERSON
        val nextOpenedValves =      if(s.node.isValve) s.openedValves+s.node.asValve     else s.openedValves
        val thisFlow =              if(s.node.isValve) s.node.flow  else 0
        val currentNode =           if(s.node.isValve) s.node.asValve.chamber            else s.node
        val nextVisitedSinceValve = if(s.node.isValve) Set(currentNode)                  else s.visitedSinceValveOpened + s.node

        // ELEPHANT
        val nextOpenedValvesEle =      if(s.elephant.isValve) nextOpenedValves+s.elephant.asValve   else nextOpenedValves
        val thisFlowEle =              if(s.elephant.isValve) s.elephant.flow  else 0
        val currentNodeEle =           if(s.elephant.isValve) s.elephant.asValve.chamber            else s.elephant
        val nextVisitedSinceValveEle = if(s.elephant.isValve) Set(currentNodeEle)                   else s.visitedSinceValveOpenedEle + s.elephant

        val nextClosedValves = volcano.valves -- nextOpenedValvesEle

        def visitable(n:Node) = !nextVisitedSinceValve.contains(n) && ( !n.isValve || !nextOpenedValvesEle.contains(n.asValve) )
        def visitableEle(n:Node) = !nextVisitedSinceValveEle.contains(n) && ( !n.isValve || !nextOpenedValvesEle.contains(n.asValve) )

        //val neigbours = currentNode.nodeNeigbours(volcano).filter(visitable)
        //val neigboursEle = currentNodeEle.nodeNeigbours(volcano).filter(visitableEle)
        // val search = for(
        //   n <- neigbours ;
        //   nEle <- neigboursEle if ( !n.isValve || !nEle.isValve || n != nEle )
        // ) yield{
        //   dfs_memoized_impl(n, nEle, nextTime, nextOpenedValvesEle, nextClosedValves, nextVisitedSinceValve, nextVisitedSinceValveEle, flowAccum+flowPerMinute, flowPerMinute+thisFlow+thisFlowEle )
        // }

        // val ret = if( search.isEmpty ){
        //   //(0, List(),List())
        //   0
        // }
        // else{
        //   //val max = search.maxBy(_._1)
        //   //(max._1+ (thisFlow+thisFlowEle)*(s.remainingTime-1), node :: max._2 , elephant :: max._3 )
        //   val max = search.max
        //   max + (thisFlow + thisFlowEle)*(s.remainingTime-1)
        // }
        // ret

        val neigbours = currentNode.nodeNeigbours(volcano)
        val neigboursEle = currentNodeEle.nodeNeigbours(volcano)
        var index = 0
        var best = 0
        while( index < neigbours.size ){
          val n = neigbours(index)
          if( visitable(n) ){
            var indexEle = 0
            while( indexEle < neigboursEle.size ){
              val nEle = neigboursEle(indexEle)
              if( visitableEle(nEle) && ( !n.isValve || !nEle.isValve || n != nEle ) ){
                val nextState = State(n, nEle, nextTime, nextOpenedValvesEle, nextClosedValves, nextVisitedSinceValve, nextVisitedSinceValveEle, s.flowAccum+s.flowPerMinute, s.flowPerMinute+thisFlow+thisFlowEle)
                val ret = dfs_memoized_impl(nextState)
                best = best max ret
              }
              indexEle += 1
            }
          }

          index += 1
        }

        best + (thisFlow + thisFlowEle)*(nextTime)

        
      }
      saveKey(compute)

      if( compute /*._1*/ > currentMax ){
        currentMax = compute /*._1 */
          println( s"currentMax: $currentMax   cache size:${memo.size}")

      }

      compute
    }
        
  }

  val ret = dfs_memoized_impl( State(start,start, time+1,Set(),volcano.valves,Set(),Set(),0,0) )
  println( s"cache size: ${memo.size}")
  ret
}


val lines = LineIterator.lineIterator( new FileInputStream("input") )
val chambers = lines.map( Chamber.fromLine ).toList
val volcano = new Volcano(chambers)

// for( c1 <- chambers ; c2 <- chambers ){
//   println( s"---- $c1 a $c2: ${volcano.distance(c1,c2)}")
// }

// for( c <- chambers ){
//   println( s"---- $c ${c.nodeNeigbours(volcano)}")
// }


val solution = LineIterator.time("solution 1"){
  dfs_memoized(volcano, volcano.chamber("AA"), 26)
}
//println(s"Solution 1: ${solution._1}\n${solution._2}\n${solution._3}")

println(s"Solution: ${solution}")
