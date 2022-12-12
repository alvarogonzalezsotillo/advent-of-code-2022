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



def search(implicit h: Hill) : Iterator[Position] = {

  val goalV = goal
  val startV = start
  val toVisit = Queue[Position](startV)
  val parents = MMap[Position,Position]()

  parents(startV) = startV

  while( !toVisit.isEmpty ){
    val p = toVisit.dequeue

    val neighbours = p.neighbours.filter(_.value <= p.value + 1)

    for( n <- neighbours if !parents.isDefinedAt(n) && !parents.isDefinedAt(goalV) ){
      parents(n) = p
      toVisit += n
    }

    if( parents.isDefinedAt(goalV) ){
      return Iterator.iterate(goalV)( (p)=>parents(p) ).takeWhile( _ != startV )
    }
  }

  return Iterator()
}


val lines = LineIterator.lineIterator( new FileInputStream("input") )
val hill : Hill = lines.map( _.toSeq ).toSeq

println( search(hill).size )
