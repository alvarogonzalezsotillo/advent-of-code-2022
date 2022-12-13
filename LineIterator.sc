// -*- mode: scala;coding:utf-8 -*-

import java.io._
import scala.util._

def time[A](name: String)( proc : => A ) = {
  val ini = System.currentTimeMillis()
  val ret = proc
  val end = System.currentTimeMillis()
  print( s"$name: ${end-ini} millis" )
  ret
}


def lineIterator( inStream : InputStream ) = new Iterator[String](){

  val reader = new BufferedReader( new InputStreamReader ( inStream ) )

  var current = reader.readLine()

  override def hasNext() = current != null

  override def next() = {
    if( current == null ){
      throw new IllegalStateException()
    }
    val ret = current
    current = reader.readLine()
    ret
  }
}
