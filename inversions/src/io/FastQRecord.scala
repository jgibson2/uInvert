package io

class FastQRecord(val id : String, val sequence : String, val qualities : String) {
  assert(sequence.length == qualities.length)
  def length() : Int = sequence.length()
  def trim(qual_cutoff : Int, length_cutoff : Int) : Option[FastQRecord] = {
    var sum = 0
    var min = 0
    var min_position = sequence.length
    for(i <- sequence.length - 1 to 0 by -1) {
      if(i < length_cutoff) {
        return None
      }
      // println(qualities.charAt(i).toInt)
      sum += qualities.charAt(i).toInt - qual_cutoff
      if(sum < min) {
        min_position = i
        min = sum
      }
      else if (sum > 0) {
        val n_loc = sequence.lastIndexOf('N')
        val front_trim = if(n_loc > -1 && n_loc < sequence.length - 1) n_loc + 1 else 0
        return Some(new FastQRecord(id, sequence.substring(front_trim, min_position), qualities.substring(front_trim,min_position)))
      }
    }
    None
  }
}
