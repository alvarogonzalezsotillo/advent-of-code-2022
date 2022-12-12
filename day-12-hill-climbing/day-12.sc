// -*- mode: scala;coding:utf-8 -*-

import java.io._
import scala.util._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue
import scala.collection.mutable.{Set => MSet,Map => MMap}
import $file.^.LineIterator

def log(s: => String) = () // println(s)

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



def search(start:Position,goal:Position, neighbourP : (Position,Position)=> Boolean )(implicit h: Hill) : Option[Map[Position,Position]] = {

  val toVisit = Queue[Position](start)
  val parents = MMap[Position,Position]()

  parents(start) = start

  while( !toVisit.isEmpty ){
    val p = toVisit.dequeue

    val neighbours = p.neighbours.filter( neighbourP(p,_) )

    for( n <- neighbours if !parents.isDefinedAt(n) && !parents.isDefinedAt(goal) ){
      parents(n) = p
      toVisit += n
    }

    if( parents.isDefinedAt(goal) ){
      return Some(parents.toMap)
    }
  }

  return None
}

{
  val lines = LineIterator.lineIterator( new FileInputStream("input") )
  implicit val hill : Hill = lines.map( _.toSeq ).toSeq

  val parents = search(start,goal, (from,to)=>to.value <= from.value + 1).get
  val solution = Iterator.iterate(goal)( (p)=>parents(p) ).takeWhile( _ != start )
  println( solution.size ) // 534
}

