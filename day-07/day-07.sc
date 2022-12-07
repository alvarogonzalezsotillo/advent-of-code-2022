// -*- mode: scala;coding:utf-8 -*-

import java.io._
import scala.util._
import scala.collection.mutable._
import $file.^.LineIterator

def log(s : => String) = ()// println(s)


val lines = LineIterator.lineIterator( new FileInputStream("input") )

val cd = "\\$ cd (.*)".r
val ls = "\\$ ls".r
val dir = "dir (.*)".r
val file = "([0-9]*) (.*)".r

trait Item{
  val name : String
  def size : Int
  val isFolder = false
  def asFolder : Folder = ???
}

case class File(val name: String, val size: Int) extends Item

class Folder( val name: String, val parent: Option[Folder] ) extends Item{

  val items = ArrayBuffer[Item]()

  def add(item:Item) = items += item

  def get(name: String) = items.find( _.name == name ).get

  def size = items.map(_.size).sum

  override def asFolder : Folder = this

  override def toString = s"$name-$size"

  override val isFolder = true
}

object Folder{
  def apply(name : String ) = new Folder(name, None)
  def apply(name : String, parent: Folder ) = new Folder(name, Some(parent) )
  def unapply( f: Folder ) : Option[String] = Some(f.name)
}

def itemIterator( item: Item ) : Iterator[Item] = item match{
  case File(_,_) => Iterator(item)
  case Folder(_)  => {
   item.asFolder.items.map(itemIterator).fold(Iterator(item))( _ ++ _ )
  }
}

 
val root = Folder("/")
var current = root

for( line <- lines ){
  line match{
    case cd("/") =>          current = root
    case cd("..") =>         current = current.parent.get
    case cd(folder) =>       current = current.get(folder).asFolder
    case dir(name) =>        current.add( Folder(name, current) )
    case file(size, name) => current.add( File(name, size.toInt ))
    case ls() =>
  }
  
}

{  
  val solution = itemIterator(root).filter(_.isFolder).filter(_.size <= 100000 ).map(_.size).sum
  println( s"Solution 1: $solution")
}


{
  val total = 70000000
  val needed = 30000000
  val free = total - root.size
  val toBeFreed = needed - free

  println( s"free: $free toBeFreed:$toBeFreed")

  val solution = itemIterator(root).filter(_.isFolder).filter(_.size >= toBeFreed ).minBy(_.size)
  println( s"Solution 2: ${solution.size}" )
}
