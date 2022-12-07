// -*- mode: scala;coding:utf-8 -*-

import java.io._
import scala.util._
import scala.collection.mutable._
import $file.^.LineIterator

def log(s : => String) = ()// println(s)



val movement = "move (.*) from (.*) to (.*)".r


def readStacks(lines: Iterator[String]) : Array[Stack[Char]] = {
  val stacks = Array.fill(9)( Stack[Char]() )

  for( l <- lines.takeWhile( _.trim != "" ) ){
    for( i <- 0 until l.size if l(i) >= 'A' && l(i) <= 'Z' ){
      val stackIndex : Int = (i-1)/4
      stacks(stackIndex).push(l(i))
    }
  }
  stacks.map( _.reverse )
}


{
  val lines = LineIterator.lineIterator( new FileInputStream("input") )
  val stacks = readStacks(lines)
  for( l <- lines ) l match{
    case movement(size,from,to) => {
      for( _ <- 0 until size.toInt ){
        stacks(to.toInt-1).push( stacks(from.toInt-1).pop() )
      }
    }
  }

  println( "Part 1")
  println( stacks.map(_.headOption).map(_.getOrElse(" ")).mkString )
}

{
  val lines = LineIterator.lineIterator( new FileInputStream("input") )
  val stacks = readStacks(lines)
  for( l <- lines ) l match{
    case movement(size,from,to) => {
      val s = Stack[Char]()
      for( _ <- 0 until size.toInt ){
        s.push( stacks(from.toInt-1).pop() )
      }
      s.foreach( stacks(to.toInt-1).push(_) )
    }
  }

  println( "Part 2")
  println( stacks.map(_.headOption).map(_.getOrElse(" ")).mkString )
}

