// -*- mode: scala;coding:utf-8 -*-

import java.io._
import scala.util._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue
import scala.collection.mutable.{Set => MSet,Map => MMap}
import $file.^.LineIterator

def log(s: => String) = ()// println(s)

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



def search(start:Position,goal:Position, neighbourP : (Position,Position)=> Boolean )(implicit h: Hill) : Either[Map[Position,Position],Map[Position,Position]] = {

  val toVisit = Queue[Position](start)
  val parents = MMap[Position,Position]()

  parents(start) = start

  while( !toVisit.isEmpty ){
    log( "toVisit:" + toVisit )
    val p = toVisit.dequeue

    val neighbours = p.neighbours.filter( neighbourP(p,_) )

    for( n <- neighbours if !parents.isDefinedAt(n) && !parents.isDefinedAt(goal) ){
      parents(n) = p
      toVisit += n
    }

    if( parents.isDefinedAt(goal) ){
      return Right(parents.toMap)
    }
  }

  return Left(parents.toMap)
}

{
  val lines = LineIterator.lineIterator( new FileInputStream("input") )
  implicit val hill : Hill = lines.map( _.toSeq ).toSeq

  val parents = search(start,goal, (from,to)=>to.value <= from.value + 1).right.getOrElse(null)
  val solution = Iterator.iterate(goal)( (p)=>parents(p) ).takeWhile( _ != start )
  println( solution.size ) // 534
}

{
  val lines = LineIterator.lineIterator( new FileInputStream("sample") )
  implicit val hill : Hill = lines.map( _.toSeq ).toSeq

  val parents = search(goal,Position(-1,-1), (from,to)=> from.value -1 <= to.value ).left.getOrElse(null)
  log( "Parents:" + parents )
  val all : Seq[Option[(Position,Seq[Position])]] = for( x <- 0 until width ; y <- 0 until height ; pos = Position(x,y) if pos.value == 'a' ) yield {
    val path = Iterator.iterate[Option[Position]](Some(pos)){ case Some(p) => parents.get(p) }.takeWhile( o => o.isDefined && o.get != goal )
    val seq = path.toSeq.map( _.get )
    if( seq.last == goal )
      Some( (pos, seq) )
    else
      None
  }
  val solution = all.filter(_.isDefined).map(_.get).filter(_._2.size > 0).minBy( _._2.size )
  println( "solution:" + solution._2.size + " -- " + solution )
}
