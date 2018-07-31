package io

import scala.collection.immutable.HashMap

object SequenceUtils{
  val char_map : HashMap[Char,Char] = HashMap[Char, Char](('A', 'T'), ('C','G'), ('G','C'), ('T','A'), ('N','N'))

  def reverseComplement(seq : String) : String = {
    seq.reverse.toIterator.map({c => char_map(c)}).mkString("")
  }
}
