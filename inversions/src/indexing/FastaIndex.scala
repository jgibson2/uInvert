package indexing

import scala.collection.mutable

class FastaIndex(val nameMap : Map[String, Int]) {
  val idxList : List[Int] = nameMap.values.toList.sorted.reverse
  val posMap : mutable.HashMap[Int, String] = new mutable.HashMap[Int, String]()
  for(s <- nameMap.keys.toStream) posMap += (nameMap(s) -> s)
  def getChromosomalPosition(position : Int) : Option[(String, Int)] = {
    val idx = idxList.find(_ <= position) match {
      case Some(x) => return Some((posMap(x), position - x))
      case None => return None
    }
    None
  }
}
