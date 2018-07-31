package io

import scala.io.Source

class FastQReader(val filename : String) extends Iterator[FastQRecord] {
  private val lines = Source.fromFile(filename).getLines().grouped(4)

  def next() : FastQRecord = {
    val rec = lines.next()
    if(rec.length < 4) {
      return new FastQRecord("", "", "")
    }
    new FastQRecord(rec(0), rec(1),rec(3))
  }

  def hasNext : Boolean = lines.hasNext
}
