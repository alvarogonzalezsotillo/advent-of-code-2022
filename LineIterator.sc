import java.io._
import scala.util._


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
