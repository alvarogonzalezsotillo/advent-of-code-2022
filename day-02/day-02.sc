// -*- mode: scala;coding:utf-8 -*-

import java.io._
import scala.util._
import $file.^.LineIterator

def log(s : => String) = println(s)




def outcome( him: String, me: String ) =  me match{
  // ROCK, PAPER, SCISSORS
  case "X" => 1 + (him match{ // ROCK
    case "A" => 3
    case "B" => 0
    case "C" => 6
  })
  case "Y" => 2 + (him match{ // paper
    case "A" => 6
    case "B" => 3
    case "C" => 0
  })
  case "Z" => 3 + (him match{ // scissors
    case "A" => 0
    case "B" => 6
    case "C" => 3
  })
}

def computeChoose( him: String, directive: String ) = directive match{
  case "X" => him match{ // loose
    case "A" => "Z"
    case "B" => "X"
    case "C" => "Y"
  }


  case "Y" => him match{ // draw
    case "A" => "X"
    case "B" => "Y"
    case "C" => "Z"
  }


  case "Z" => him match{ // win
    case "A" => "Y"
    case "B" => "Z"
    case "C" => "X"
    
  }
}


{
  val lines = LineIterator.lineIterator( new FileInputStream("input") )
  val result = lines.map( _.split(" ") ).foldLeft(0){ case (accum,Array(him,me)) => accum + outcome(him,me) }
  println( "First pass: " + result )
}

{
  val lines =  LineIterator.lineIterator( new FileInputStream("input") )
  val result = lines.map( _.split(" ") ).foldLeft(0){ case (accum,Array(him,directive)) => accum + outcome(him,computeChoose(him,directive)) }
  println( "Second pass: " + result )
}
