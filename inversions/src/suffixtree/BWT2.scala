package suffixtree

class BWT2(val fwd_bwt : String, val rev_bwt : String, val decimation_factor : Int, val alphabet : Map[Char, Int]) {

  // occ and C arrays
  val alphabet_size : Int = alphabet.size
  val fwd_occ : Array[Array[Int]] = Array.ofDim[Int](alphabet_size, fwd_bwt.length / decimation_factor + 1)
  val rev_occ : Array[Array[Int]] = Array.ofDim[Int](alphabet_size, rev_bwt.length / decimation_factor + 1)
  val fwd_C : Array[Int] = Array.ofDim[Int](alphabet_size)
  val rev_C : Array[Int] = Array.ofDim[Int](alphabet_size)
  populate_arrays()

  def populate_arrays() : Unit = {
    val fwd_counter = Array.ofDim[Int](alphabet_size)
    val rev_counter = Array.ofDim[Int](alphabet_size)
    for(i <- 0 until fwd_bwt.length) {
      //keep track of chars, assigning the occ array when we reach a point where we are storing the result
      val c : Int = alphabet.getOrElse(fwd_bwt.charAt(i), -1)
      if(c != -1) fwd_counter(c) += 1 //get the character and add to the counter

      if(i % decimation_factor == 0) { //every (decimation_factor)-th suffix
        for(c_idx <- 0 until alphabet_size) {
          fwd_occ(c_idx)(i / decimation_factor) = fwd_counter(c_idx)
        }
      }
    }
    // do the same thing for reverse
    for(i <- 0 until rev_bwt.length) {
      val c : Int = alphabet.getOrElse(rev_bwt.charAt(i), -1)
      if(c != -1) rev_counter(c) += 1

      if(i % decimation_factor == 0) {
        for(c_idx <- 0 until alphabet_size) {
          rev_occ(c_idx)(i / decimation_factor) = rev_counter(c_idx)
        }
      }
    }
    //populate the C array
    fwd_C(0) = 0
    rev_C(0) = 0
    for(c_idx <- 1 until alphabet_size) {
      fwd_C(c_idx) = fwd_C(c_idx - 1) + fwd_counter(c_idx - 1) //previous value + whatever the # of previous characters there are in the BWT
      rev_C(c_idx) = rev_C(c_idx - 1) + rev_counter(c_idx - 1)
    }
  }

  def extend_interval(I : BiInterval, c : Char) : BiInterval = {
    // compute new forward interval
    val fwd_l = C(c) + occ(fwd_bwt, fwd_occ, c, I.fwd_l - 1)
    val fwd_r = C(c) + occ(fwd_bwt, fwd_occ, c, I.fwd_r) - 1

    /*
    – x = How many suffixes in [i,j] are preceded by chars < c?
    – y = How many suffixes in [i,j] are preceded by chars ≤ c?

    DON'T FORGET ABOUT $
     */
    val x = {
      var r = 0
      val c_idx = alphabet.getOrElse(c, -1)
      for( i <- 0 until c_idx) {
        r += (occ_c_value(fwd_bwt, fwd_occ, i, I.fwd_r) - occ_c_value(fwd_bwt, fwd_occ, i, I.fwd_l - 1))
      }
      r
    }

    val y = {
      val c_idx = alphabet.getOrElse(c, -1)
      x + (occ_c_value(fwd_bwt, fwd_occ, c_idx, I.fwd_r) - occ_c_value(fwd_bwt, fwd_occ, c_idx, I.fwd_l - 1))
    }

    val rev_l = I.rev_l + x
    val rev_r = I.rev_l + y - 1

    //assert(rev_r - rev_l == fwd_r - fwd_l)
    val ival = new BiInterval(fwd_l, fwd_r, rev_l, rev_r, I.depth + 1)
    ival //return new interval
  }

  def extend_interval(I : ScoredBiInterval, c : Char, match_bonus : Int = 1) : ScoredBiInterval = {
    // compute new forward interval
    val fwd_l = C(c) + occ(fwd_bwt, fwd_occ, c, I.fwd_l - 1)
    val fwd_r = C(c) + occ(fwd_bwt, fwd_occ, c, I.fwd_r) - 1

    /*
    – x = How many suffixes in [i,j] are preceded by chars < c?
    – y = How many suffixes in [i,j] are preceded by chars ≤ c?

    DON'T FORGET ABOUT $
     */
    val x = {
      var r = 0
      val c_idx = alphabet.getOrElse(c, -1)
      for( i <- 0 until c_idx) {
        r += (occ_c_value(fwd_bwt, fwd_occ, i, I.fwd_r) - occ_c_value(fwd_bwt, fwd_occ, i, I.fwd_l - 1))
      }
      r
    }

    val y = {
      val c_idx = alphabet.getOrElse(c, -1)
      x + (occ_c_value(fwd_bwt, fwd_occ, c_idx, I.fwd_r) - occ_c_value(fwd_bwt, fwd_occ, c_idx, I.fwd_l - 1))
    }

    val rev_l = I.rev_l + x
    val rev_r = I.rev_l + y - 1

    //assert(rev_r - rev_l == fwd_r - fwd_l)
    val ival = new ScoredBiInterval(fwd_l, fwd_r, rev_l, rev_r, I.depth + 1, I.score + match_bonus, I.differences)
    ival //return new interval
  }

  def extend_interval_backwards(I : BiInterval, c : Char) : BiInterval = {
    // compute new backward interval
    val rev_l = C(c) + occ(rev_bwt, rev_occ, c, I.rev_l - 1)
    val rev_r = C(c) + occ(rev_bwt, rev_occ, c, I.rev_r) - 1

    /*
    – x = How many suffixes in [i,j] are preceded by chars < c?
    – y = How many suffixes in [i,j] are preceded by chars ≤ c?

    DON'T FORGET ABOUT $
     */
    val x = {
      var r = 0
      val c_idx = alphabet.getOrElse(c, -1)
      for( i <- 0 until c_idx) {
        r += (occ_c_value(rev_bwt, rev_occ, i, I.rev_r) - occ_c_value(rev_bwt, rev_occ, i, I.rev_l - 1))
      }
      r
    }

    val y = {
      val c_idx = alphabet.getOrElse(c, -1)
      x + (occ_c_value(rev_bwt, rev_occ, c_idx, I.rev_r) - occ_c_value(rev_bwt, rev_occ, c_idx, I.rev_l - 1))
    }

    val fwd_l = I.fwd_l + x
    val fwd_r = I.fwd_l + y - 1

    //assert(rev_r - rev_l == fwd_r - fwd_l)
    val ival = new BiInterval(fwd_l, fwd_r, rev_l, rev_r, I.depth + 1)
    ival //return new interval
  }

  def is_right_diverse(I : BiInterval) : Boolean = {
    num_right_diverse_chars(I) >= 2
  }

  def num_right_diverse_chars(I : BiInterval) : Int = {
    var distinct_chars = 0
    for(c <- alphabet.keys) {
      if(occ(rev_bwt, rev_occ, c, I.rev_r) - occ(rev_bwt, rev_occ, c, I.rev_l - 1) > 0) {
        distinct_chars += 1
      }
    }
    distinct_chars
  }

  def occ(bwt : String, occ_arr : Array[Array[Int]], c : Char, i : Int) : Integer = {
    if(i < 0) return 0
    val prev_occ_index = i / decimation_factor
    val counter = Array.ofDim[Int](alphabet_size)
    for(j <- prev_occ_index * decimation_factor + 1 to i) {
      val c : Int = alphabet.getOrElse(bwt.charAt(j), -1)
      if (c != -1) counter(c) += 1
    }
    val c_idx : Int = alphabet.getOrElse(c, -1)
    if (c_idx != -1) {
      val res = occ_arr(c_idx)(prev_occ_index) + counter(c_idx)
      res
    } // return occ value
    else {
      0
    }
  }

  def occ_c_value(bwt : String, occ_arr : Array[Array[Int]], c_idx : Int, i : Int) : Integer = {
    if(i < 0) return 0
    val prev_occ_index = i / decimation_factor
    val counter = Array.ofDim[Int](alphabet_size)
    for(j <- prev_occ_index * decimation_factor + 1 to i) {
      val c : Int = alphabet.getOrElse(bwt.charAt(j), -1)
      if (c != -1) counter(c) += 1
    }
    if (c_idx != -1) {
      val res = occ_arr(c_idx)(prev_occ_index) + counter(c_idx)
      res
    } // return occ value
    else {
      0
    }
  }

  def C(c : Char) : Int = {
    val c_idx : Int = alphabet.getOrElse(c, -1)
    if(c != -1) fwd_C(c_idx) else 0
  }


  def has_child(I : BiInterval, c : Char) : Boolean = {
    (occ(fwd_bwt, fwd_occ, c, I.fwd_r) - 1) - occ(fwd_bwt, fwd_occ, c, I.fwd_l - 1) > 0
  }

  // taken from https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2705234/ Figure 3
  def BWA_calculate_D(P : String, match_bonus : Int = 1) : Array[Int] = {
    val P_rev = P.reverse
    val D = Array.ofDim[Int](P.length)
    var k = 0
    var l = fwd_bwt.length - 1
    var z = 0
    for(i <- 0 until P.length) {
      k = C(P_rev.charAt(i)) + occ(fwd_bwt, fwd_occ, P_rev.charAt(i), k - 1) // forward or reverse occ?
      l = C(P_rev.charAt(i)) + occ(fwd_bwt, fwd_occ, P_rev.charAt(i), l) - 1
      if(k > l) {
        k = 0
        l = fwd_bwt.length - 1
        z += 1
      }
      D(i) = (P.length - (i + z)) * match_bonus
    }
    D
  }

  def BWA_calculate_D_backwards(P : String, match_bonus : Int = 1) : Array[Int] = {
    val D = Array.ofDim[Int](P.length)
    var k = 0
    var l = fwd_bwt.length - 1
    var z = 0
    for(i <- 0 until P.length) {
      k = C(P.charAt(i)) + occ(rev_bwt, rev_occ, P.charAt(i), k - 1)
      l = C(P.charAt(i)) + occ(rev_bwt, rev_occ, P.charAt(i), l) - 1
      if(k > l) {
        k = 0
        l = fwd_bwt.length - 1
        z += 1
      }
      D(i) = (i - z) * match_bonus
    }
    D
  }

  def calculate_D(P : String, match_bonus : Int = 1) : Array[Int] = {
    //val D = (for(i <- 0 until P.length) yield match_bonus * (P.length - i)).toArray
    val D = Array.ofDim[Int](P.length)
    for(i <- 0 until P.length) {
      var I = new BiInterval(0, fwd_bwt.length - 1, 0, rev_bwt.length - 1, 0)
      while (I.length() > 0 && I.depth + i < P.length) {
        // do something with I
        I = extend_interval_backwards(I, P.charAt(I.depth + i))
      }
      D(i) = I.depth * match_bonus
    }
    D
  }

  def extend_scored_mismatched_interval(I : ScoredBiInterval, c : Char, pattern_c : Char, match_bonus : Int = 1, mismatch_penalty : Int = -1) : ScoredBiInterval = {
    // compute new forward interval
    val fwd_l = C(c) + occ(fwd_bwt, fwd_occ, c, I.fwd_l - 1)
    val fwd_r = C(c) + occ(fwd_bwt, fwd_occ, c, I.fwd_r) - 1

    /*
    – x = How many suffixes in [i,j] are preceded by chars < c?
    – y = How many suffixes in [i,j] are preceded by chars ≤ c?

    DON'T FORGET ABOUT $
     */
    val x = {
      var r = 0
      val c_idx = alphabet.getOrElse(c, -1)
      for( i <- 0 until c_idx) {
        r += (occ_c_value(fwd_bwt, fwd_occ, i, I.fwd_r) - occ_c_value(fwd_bwt, fwd_occ, i, I.fwd_l - 1))
      }
      r
    }

    val y = {
      val c_idx = alphabet.getOrElse(c, -1)
      x + (occ_c_value(fwd_bwt, fwd_occ, c_idx, I.fwd_r) - occ_c_value(fwd_bwt, fwd_occ, c_idx, I.fwd_l - 1))
    }

    val rev_l = I.rev_l + x
    val rev_r = I.rev_l + y - 1

    //assert(rev_r - rev_l == fwd_r - fwd_l)
    val ival = new ScoredBiInterval(fwd_l, fwd_r, rev_l, rev_r, I.depth + 1, I.score + (if(c == pattern_c) match_bonus else mismatch_penalty), I.differences + (if(c == pattern_c) 0 else 1))
    ival //return new interval
  }

  def extend_scored_mismatched_interval_backwards(I : ScoredBiInterval, c : Char, pattern_c : Char, match_bonus : Int = 1, mismatch_penalty : Int = -1) : ScoredBiInterval = {
    // compute new backward interval
    val rev_l = C(c) + occ(rev_bwt, rev_occ, c, I.rev_l - 1)
    val rev_r = C(c) + occ(rev_bwt, rev_occ, c, I.rev_r) - 1

    /*
    – x = How many suffixes in [i,j] are preceded by chars < c?
    – y = How many suffixes in [i,j] are preceded by chars ≤ c?

    DON'T FORGET ABOUT $
     */
    val x = {
      var r = 0
      val c_idx = alphabet.getOrElse(c, -1)
      for( i <- 0 until c_idx) {
        r += (occ_c_value(rev_bwt, rev_occ, i, I.rev_r) - occ_c_value(rev_bwt, rev_occ, i, I.rev_l - 1))
      }
      r
    }

    val y = {
      val c_idx = alphabet.getOrElse(c, -1)
      x + (occ_c_value(rev_bwt, rev_occ, c_idx, I.rev_r) - occ_c_value(rev_bwt, rev_occ, c_idx, I.rev_l - 1))
    }

    val fwd_l = I.fwd_l + x
    val fwd_r = I.fwd_l + y - 1

    //assert(rev_r - rev_l == fwd_r - fwd_l)
    val ival = new ScoredBiInterval(fwd_l, fwd_r, rev_l, rev_r, I.depth + 1, I.score + (if(c == pattern_c) match_bonus else mismatch_penalty), I.differences + (if(c == pattern_c) 0 else 1))
    ival //return new interval
  }
}
