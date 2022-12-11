// -*- mode: scala;coding:utf-8 -*-

import java.io._
import scala.util._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{Set => MSet}
import $file.^.LineIterator

def operation(old:Long, operator:String, operand:String) = {
  val operandI = operand match{
    case "old" => old
    case _ => operand.toLong  
  }
  operator match{
    case "+" => old + operandI
    case "-" => old - operandI
    case "*" => old * operandI
  }
}


class Monkey(){
  var operand = "???"
  var operation = "???"
  var divider: Int = 0
  var ifTrue: Int = 0
  var ifFalse: Int = 0
  val items = ArrayBuffer[Long]()
  def test(i:Long) = {
    inspectedItems += 1
    if( i % divider == 0 ) ifTrue else ifFalse
  }
  var inspectedItems = 0
  override def toString = s" ${items.mkString(",")} ($inspectedItems) ($operation $operand $divider ? $ifTrue : $ifFalse)"
}


def parseMonkeys( in: Iterator[String] ) = {
  val monkey = "Monkey (.*):".r
  val items = " *Starting items: (.*)".r
  val operation = """ *Operation: new = old (\+|\*) (.*)""".r
  val test = " *Test: divisible by (.*)".r
  val ifTrue = " *If true: throw to monkey (.*)".r
  val ifFalse = " *If false: throw to monkey (.*)".r

  val ret = ArrayBuffer[Monkey]()

  var current : Monkey = null
  for( l <- in ) l match{
    case monkey(_) => current = new Monkey()
    case items(its) => current.items ++= its.split(",").map(_.trim.toLong)
    case operation(op1,op2) => {
      current.operand = op2
      current.operation = op1
    }
    case test(divider) => current.divider = divider.toInt
    case ifTrue(m) => current.ifTrue = m.toInt  
    case ifFalse(m) => {
      current.ifFalse = m.toInt
      ret += current
      current = null
    }
    case _ => 
  }

  ret.toSeq
}

def mcd( a: Int, b: Int ) : Int = (a min b) match{
  case 0 => a max b
  case n => mcd( a min b, (a max b)%(a min b))
}

def cycle( monkeys: Seq[Monkey], divider: Int = 3, modulus: Int = Int.MaxValue ){
  for( m <- 0 until monkeys.size ; i <- 0 until monkeys(m).items.size ){
    val item = monkeys(m).items.remove(0)
    val inspectedItem = operation( item, monkeys(m).operation, monkeys(m).operand)/divider
    val newMonkey = monkeys(m).test(inspectedItem)
    monkeys(newMonkey).items += inspectedItem%modulus
  }
}


if( true ){
  val lines = LineIterator.lineIterator( new FileInputStream("input") )
  val monkeys = parseMonkeys(lines)
  for( round <- 1 to 20 ){
    cycle(monkeys)
    println(s"\nRound $round")
    println( monkeys.mkString("\n") )
  }
  val solution = monkeys.sortBy( _.inspectedItems*(-1) ).take(2).map( _.inspectedItems )
  println( s"Solution 1: ${solution(0) * solution(1)}") //66802
}

if( true ){
  val lines = LineIterator.lineIterator( new FileInputStream("input") )
  val monkeys = parseMonkeys(lines)
  val mcd_ = monkeys.map( _.divider ).foldLeft(monkeys(0).divider)( mcd(_,_) )
  val mcm = monkeys.map( _.divider ).product / mcd_
  for( round <- 1 to 10000 ){
    cycle(monkeys,1,mcm)
    println(s"\nRound $round")
    println( monkeys.mkString("\n") )
  }
  val solution = monkeys.sortBy( _.inspectedItems*(-1) ).take(2).map( _.inspectedItems.toLong )
  println( s"Solution 2: ${solution(0) * solution(1)}  $mcd_ $mcm" ) //21800916620
}
