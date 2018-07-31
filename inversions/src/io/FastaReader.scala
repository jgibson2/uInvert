package io
import scala.collection.mutable
import scala.io.Source

class FastaReader(val filename: String) {
  val text : Iterator[String] = Source.fromFile(filename).getLines()
  var sequence : String = ""
  var previdx : Int = 0
  val nameMap : mutable.HashMap[String, Int] = new mutable.HashMap()
  val posMap : mutable.HashMap[Int, String] = new mutable.HashMap()
  val firstline : String = text.next()
  nameMap += (firstline.trim.substring(1) -> previdx)
  posMap -> (previdx -> firstline.trim.substring(1))
  for(line <- text if line.length > 0) {
    if(line(0) != '>') {
      sequence += line.trim.toUpperCase.replaceAll("[^ATCGN]", "")
    }
    else {
      previdx = sequence.length // - 1 ?
      nameMap += (line.trim.substring(1) -> previdx)
      posMap -> (previdx -> line.trim.substring(1))
    }
  }
  val idxList : List[Int] = nameMap.values.toList.sorted.reverse

  def getChromosomalPosition(position : Int) : Option[(String, Int)] = {
    val idx = idxList.find(_ <= position) match {
      case Some(x) => return Some((posMap(x), position - x))
      case None => return None
    }
    None
  }
}
