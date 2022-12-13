// -*- mode: scala;coding:utf-8 -*-

import java.io._
import scala.util._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue
import scala.collection.mutable.{Set => MSet,Map => MMap}
import $file.^.LineIterator

def log(s: => String) = println(s)

type Hill = Seq[Seq[Char]]
def start(implicit h: Hill) = find('S').head
def goal(implicit h: Hill) = find('E').head
def width(implicit h: Hill) = h(0).size
def height(implicit h: Hill) = h.size

case class Position(x:Int, y:Int){

  def char(implicit h: Hill) = h(y)(x)

  def value(implicit h: Hill) = char match{
    case 'S' => 'a'
    case 'E' => 'z'
    case c => c
  }

  def insideHill(implicit h: Hill) = x >= 0 && y >= 0 && x < width && y < height
  def neighbours(implicit h: Hill) = {
    Seq( Position(x-1,y), Position(x+1,y), Position(x,y-1), Position(x,y+1) ).
      filter( _.insideHill )
  }
}

def find(char:Char)( implicit h: Hill) = {
  for( x <- 0 until width ; y <- 0 until height ; p = Position(x,y) if p.char == char ) yield p
}


def search(goal:Position)(implicit h: Hill) = {

  val toVisit = Queue[Position](goal)
  val parents = MMap[Position,Position]()

  parents(goal) = goal

  while( !toVisit.isEmpty ){
    val p = toVisit.dequeue
    val neighbours = p.neighbours.filter( n => n.value >= p.value-1  )
    for( n <- neighbours if !parents.isDefinedAt(n) ){
      parents(n) = p
      toVisit += n
    }
  }

  parents.toMap
}

def pathFrom( parents: Map[Position,Position], pos:Position, goal: Position) = {
  var previous : Position = null

  Iterator.iterate[Option[Position]](Some(pos)){
    case Some(p)=> parents.get(p) }.
    takeWhile( _.isDefined ).
    takeWhile( p => if( previous == goal ) false else {previous=p.get; true} ).
    map(_.get).toSeq
}

{
  val lines = LineIterator.lineIterator( new FileInputStream("input") )
  implicit val hill : Hill = lines.map( _.toSeq ).toSeq

  val goalV = goal
  val parents = search(goalV)
  val solution1 = pathFrom( parents, start, goalV )
  println( solution1.size-1 ) // 534

  val paths = for( x <- 0 until width ; y <- 0 until height ; pos = Position(x,y) if pos.value=='a' ) yield{
    pathFrom(parents,pos,goalV)
  }

  val solution2 = paths.filter( _.last == goalV )
  println( solution2.minBy(_.size).size-1 ) // 525
}

