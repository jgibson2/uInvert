package suffixtree
import io.SequenceUtils

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

class BWTSuffixTree(val bwt2 : BWT2) {

  def enumerate_tree() : Unit = {
    var I = new BiInterval(0, bwt2.fwd_bwt.length - 1, 0, bwt2.rev_bwt.length - 1, 0)
    val stack = new mutable.ArrayStack[BiInterval]()
    stack push I
    while (stack.nonEmpty) {
      I = stack.pop()
      println(" | " * I.depth + s"Interval from ${I.fwd_l} to ${I.fwd_r} at depth ${I.depth} (length ${I.fwd_r - I.fwd_l + 1}) [Rev: ${I.rev_l} to ${I.rev_r}]")
      // do something with I
      var extended_intervals = for(c <- bwt2.alphabet.keys if bwt2.is_right_diverse(bwt2.extend_interval(I, c))) yield bwt2.extend_interval(I, c)

      if(extended_intervals.nonEmpty) {
        val max_ival = extended_intervals.max
        extended_intervals = for (i <- extended_intervals if i != max_ival) yield i
        stack push max_ival

        for (ival <- extended_intervals) {
          stack push ival
        }
      }
    }
  }

  def find_approximate_match(P : String, d : Int, match_bonus : Int = 1, mismatch_penalty : Int = -1, gap_penalty : Int = -2, k : Int = 3) : Option[List[ScoredBiInterval]] = {
    /*
    Variant of A* algorithm: https://www.geeksforgeeks.org/a-search-algorithm/ as reference
    Linear gap penalty for now, could make affine but I have bigger fish to fry
     */
    val P_rev = P.reverse
    val D = bwt2.BWA_calculate_D(P, match_bonus)
    //val D = (for(i <- 0 until P.length) yield match_bonus * (P.length - i)).toArray
    var I = new ScoredBiInterval(0, bwt2.fwd_bwt.length - 1, 0, bwt2.rev_bwt.length - 1, 0, 0, 0)
    val open_set = mutable.HashSet[ScoredBiInterval]()
    val closed_set = mutable.HashSet[ScoredBiInterval]()
    val matches = ListBuffer[ScoredBiInterval]()
    val f = mutable.HashMap[ScoredBiInterval, Int]()
    val g = mutable.HashMap[ScoredBiInterval, Int]()
    def compare_children(a: ScoredBiInterval, b : ScoredBiInterval) : Int = {
        f.getOrElse(a, 0).compare(f.getOrElse(b,0))
    }
    val open_queue = mutable.PriorityQueue[ScoredBiInterval]()(compare_children)
    open_queue += I
    f += (I -> D(0))
    while(open_queue.nonEmpty) {
      I = open_queue.dequeue()
      closed_set += I
      open_set -= I
      for (c <- bwt2.alphabet.keys if c != '$') {
        val child = bwt2.extend_scored_mismatched_interval(I, c, P_rev.charAt(I.depth))
        if (child.depth == P.length && child.length > 0 && child.score >= 0 && child.differences <= d) {
          matches.prepend(child)
          if(matches.size == k) {
            return Some(matches.toList)
          }
        }
        else if (child.differences <= d && child.length() > 0 && child.score >= 0) {
          if(closed_set.contains(child)) {
            //pass
          }
          else if(!open_set.contains(child)) {
            f += (child -> (child.score + D(child.depth)))
            open_queue += child
            open_set += child
              // gap in pattern
            val gap_child = new ScoredBiInterval(child.fwd_l, child.fwd_r, child.rev_l, child.rev_r, I.depth, child.score + gap_penalty, I.differences + 1)
            f += (gap_child -> (gap_child.score + D(gap_child.depth)))
            open_queue += gap_child
            open_set += gap_child
          }
        }
      }
      if(I.depth < P.length - 1) {
        val gap_child = new ScoredBiInterval(I.fwd_l, I.fwd_r, I.rev_l, I.rev_r, I.depth + 1, I.score + gap_penalty, I.differences + 1)
        f += (gap_child -> (gap_child.score + D(gap_child.depth)))
        open_queue += gap_child
        open_set += gap_child
      }

    }
    if(matches.nonEmpty) Some(matches.toList) else None
  }

  def find_exact_match(P : String) : Option[BiInterval] = {
    val P_rev = P.reverse
    var I = new BiInterval(0, bwt2.fwd_bwt.length - 1, 0, bwt2.rev_bwt.length - 1, 0)
    while (I.length() > 0 && I.depth < P.length) {
      // do something with I
      I = bwt2.extend_interval(I, P_rev.charAt(I.depth))
    }
    if(I.depth == P.length) {
      Some(I)
    }
    else None
  }

  def find_exact_match_2(P : String) : Option[BiInterval] = {
    var I = new BiInterval(0, bwt2.fwd_bwt.length - 1, 0, bwt2.rev_bwt.length - 1, 0)
    while (I.length() > 0 && I.depth < P.length) {
      // do something with I
      I = bwt2.extend_interval_backwards(I, P.charAt(I.depth))
    }
    if(I.depth == P.length) {
      Some(I)
    }
    else None
  }

  def find_exact_reverse_anchor(P : String, anchor_length : Int = 18, clip : Int = 0) : Option[BiInterval] = {
    if(P.length < anchor_length + clip) return None
    val P_rev = P.substring(P.length - anchor_length - clip, P.length - clip)
    find_exact_match_2(P_rev)
  }

  def find_exact_forward_anchor(P : String, anchor_length : Int = 18, clip : Int = 0) : Option[BiInterval] = {
    if(P.length < anchor_length + clip) return None
    val P_fwd= P.substring(clip, anchor_length + clip)
    find_exact_match_2(P_fwd)
  }

  def find_reverse_anchor(P : String, d : Int, match_bonus : Int = 1, mismatch_penalty : Int = -1, report : Int = 5, anchor_length : Int = 18, clip : Int = 0) : Option[List[ScoredBiInterval]] = {
    /*
    Variant of A* algorithm: https://www.geeksforgeeks.org/a-search-algorithm/ as reference
    Linear gap penalty for now, could make affine but I have bigger fish to fry
     */
    if(P.length < anchor_length + clip) return None
    val P_rev = P.substring(P.length - anchor_length - clip, P.length - clip)
    find_exact_match_2(P_rev) match {
      case Some(x) => return Some(List[ScoredBiInterval](new ScoredBiInterval(x.fwd_l, x.fwd_r, x.rev_l, x.rev_r, x.depth, match_bonus*P_rev.length, 0)))
      case None => //pass
    }
    val D = bwt2.BWA_calculate_D_backwards(P_rev, match_bonus)
    var I = new ScoredBiInterval(0, bwt2.fwd_bwt.length - 1, 0, bwt2.rev_bwt.length - 1, 0, 0, 0)
    val open_set = mutable.HashSet[ScoredBiInterval]()
    val closed_set = mutable.HashSet[ScoredBiInterval]()
    val matches = ListBuffer[ScoredBiInterval]()
    val f = mutable.HashMap[ScoredBiInterval, Int]()
    val g = mutable.HashMap[ScoredBiInterval, Int]()
    def compare_children(a: ScoredBiInterval, b : ScoredBiInterval) : Int = {
      f.getOrElse(b, 0).compare(f.getOrElse(a,0))
    }
    val open_queue = mutable.PriorityQueue[ScoredBiInterval]()(compare_children)
    open_queue += I
    f += (I -> D(0))
    while(open_queue.nonEmpty) {
      I = open_queue.dequeue()
      closed_set += I
      open_set -= I
      for (c <- bwt2.alphabet.keys if c != '$') {
        val child = bwt2.extend_scored_mismatched_interval_backwards(I, c, P.charAt(I.depth))
        if (child.differences <= d && child.length > 0  && child.score >= 0 && child.depth == anchor_length) {
          matches.append(child)
          if(matches.size == report) {
            return Some(matches.toList)
          }
        }
        else if (child.differences <= d && child.length() > 0 && child.score >= 0) {
          if(closed_set.contains(child)) {
            //pass
          }
          else if(!open_set.contains(child)) {
            f += (child -> (child.score + D(child.depth)))
            open_queue += child
            open_set += child

          }
        }
      }

    }
    if(matches.nonEmpty) Some(matches.toList) else None
  }

  def find_forward_anchor(P : String, d : Int, match_bonus : Int = 1, mismatch_penalty : Int = -1, report : Int = 5, anchor_length : Int = 18, clip : Int = 0) : Option[List[ScoredBiInterval]] = {
    /*
    Variant of A* algorithm: https://www.geeksforgeeks.org/a-search-algorithm/ as reference
    Linear gap penalty for now, could make affine but I have bigger fish to fry
     */

    if(P.length < anchor_length + clip) return None
    val P_fwd = P.substring(clip, anchor_length + clip)
    find_exact_match_2(P_fwd) match {
      case Some(x) => return Some(List[ScoredBiInterval](new ScoredBiInterval(x.fwd_l, x.fwd_r, x.rev_l, x.rev_r, x.depth, match_bonus*P_fwd.length, 0)))
      case None => //pass
    }
    val D = bwt2.BWA_calculate_D_backwards(P_fwd, match_bonus)
    var I = new ScoredBiInterval(0, bwt2.fwd_bwt.length - 1, 0, bwt2.rev_bwt.length - 1, 0, 0, 0)
    val open_set = mutable.HashSet[ScoredBiInterval]()
    val closed_set = mutable.HashSet[ScoredBiInterval]()
    val matches = ListBuffer[ScoredBiInterval]()
    val f = mutable.HashMap[ScoredBiInterval, Int]()
    val g = mutable.HashMap[ScoredBiInterval, Int]()
    def compare_children(a: ScoredBiInterval, b : ScoredBiInterval) : Int = {
      f.getOrElse(b, 0).compare(f.getOrElse(a,0))
    }
    val open_queue = mutable.PriorityQueue[ScoredBiInterval]()(compare_children)
    open_queue += I
    f += (I -> D(0))
    while(open_queue.nonEmpty) {
      I = open_queue.dequeue()
      closed_set += I
      open_set -= I
      for (c <- bwt2.alphabet.keys if c != '$') {
        val child = bwt2.extend_scored_mismatched_interval_backwards(I, c, P.charAt(I.depth))
        if (child.differences <= d && child.length > 0  && child.score >= 0 && child.depth == anchor_length) {
          matches.append(child)
          if(matches.size == report) {
            return Some(matches.toList)
          }
        }
        else if (child.differences <= d && child.length() > 0 && child.score >= 0) {
          if(closed_set.contains(child)) {
            //pass
          }
          else if(!open_set.contains(child)) {
            f += (child -> (child.score + D(child.depth)))
            open_queue += child
            open_set += child

          }
        }
      }

    }
    if(matches.nonEmpty) Some(matches.toList) else None
  }

  def find_approximate_match_staged(P : String, d : Int, match_bonus : Int = 1, mismatch_penalty : Int = -1, gap_penalty : Int = -2, report : Int = 2, seed_length : Int = 4) : Option[List[ScoredBiInterval]] = {
    /*
    Variant of A* algorithm: https://www.geeksforgeeks.org/a-search-algorithm/ as reference
    Linear gap penalty for now, could make affine but I have bigger fish to fry
     */
    find_exact_match(P) match {
      case Some(x) => return Some(List[ScoredBiInterval](new ScoredBiInterval(x.fwd_l, x.fwd_r, x.rev_l, x.rev_r, x.depth, match_bonus*P.length, 0)))
      case None => //pass
    }
    val P_rev = P.reverse
    val D = bwt2.BWA_calculate_D(P, match_bonus)
    var I = new ScoredBiInterval(0, bwt2.fwd_bwt.length - 1, 0, bwt2.rev_bwt.length - 1, 0, 0, 0)
    val open_set = mutable.HashSet[ScoredBiInterval]()
    val gapped_set = mutable.HashSet[ScoredBiInterval]()
    val closed_set = mutable.HashSet[ScoredBiInterval]()
    val matches = ListBuffer[ScoredBiInterval]()
    val f = mutable.HashMap[ScoredBiInterval, Int]()
    val g = mutable.HashMap[ScoredBiInterval, Int]()
    def compare_children(a: ScoredBiInterval, b : ScoredBiInterval) : Int = {
      f.getOrElse(b, 0).compare(f.getOrElse(a,0))
    }
    val open_queue = mutable.PriorityQueue[ScoredBiInterval]()(compare_children)
    val gapped_queue = mutable.PriorityQueue[ScoredBiInterval]()(compare_children)
    open_queue += I
    f += (I -> D(0))
    while(open_queue.nonEmpty || gapped_queue.nonEmpty) {
      if(open_queue.nonEmpty) {
        I = open_queue.dequeue()
      }
      else {
        I = gapped_queue.dequeue()
      }
      closed_set += I
      open_set -= I
        for (c <- bwt2.alphabet.keys if c != '$') {
          val child = bwt2.extend_scored_mismatched_interval(I, c, P_rev.charAt(I.depth))
          if (child.depth == P.length && child.length > 0 && child.score >= 0 && child.differences <= d) {
            matches.append(child)
            if (matches.size == report) {
              return Some(matches.toList)
            }
          }
          else if (child.differences <= d && child.length() > 0 && child.score >= 0) {
            if (closed_set.contains(child)) {
              //pass
            }
            else if (!open_set.contains(child)) {
              f += (child -> (child.score + D(child.depth)))
              open_queue += child
              open_set += child
              // gap in pattern
              if (I.depth > seed_length) {
                val gap_child = new ScoredBiInterval(child.fwd_l, child.fwd_r, child.rev_l, child.rev_r, I.depth, child.score + gap_penalty, I.differences + 1)
                f += (gap_child -> (gap_child.score + D(gap_child.depth)))
                gapped_queue += gap_child
                gapped_set += gap_child
              }
            }
          }
        }
        if (I.depth > seed_length && I.depth < P.length - 1) {
          val gap_child = new ScoredBiInterval(I.fwd_l, I.fwd_r, I.rev_l, I.rev_r, I.depth + 1, I.score + gap_penalty, I.differences + 1)
          f += (gap_child -> (gap_child.score + D(gap_child.depth)))
          gapped_queue += gap_child
          gapped_set += gap_child
        }
    }
    if(matches.nonEmpty) Some(matches.toList) else None
  }

  def find_inversion(P : String, d : Int, suffixArray : Array[Int], anchor_d : Int = 1, match_bonus : Int = 1, mismatch_penalty : Int = -1, gap_penalty : Int = -2,
                     report : Int = 2, seed_length : Int = 4, anchor_length : Int = 18, max_size_multiplier : Int = 3, front_clip : Int = 0, end_clip : Int = 0) : Option[List[ScoredBiInterval]] = {
    val fwd_anchors = find_forward_anchor(P, anchor_d, match_bonus, mismatch_penalty, anchor_length = anchor_length, clip = front_clip) match {
      case Some(x) => x
      case None => return None
    }
    val rev_anchors = find_reverse_anchor(P, anchor_d, match_bonus, mismatch_penalty, anchor_length = anchor_length, clip = end_clip) match {
      case Some(x) => x
      case None => return None
    }

    val matches = new ListBuffer[ScoredBiInterval]
    for(i <- fwd_anchors) {
      for(j <- rev_anchors) {
        for(rev <- j.fwd_l to j.fwd_r) {
          for (fwd <- i.fwd_l to i.fwd_l) {
            if (suffixArray(rev) - suffixArray(fwd) <= P.length * max_size_multiplier) {
              //TODO: add tunable parameters to call
              val s_match_list = find_inversion_site(P, d, anchor_length, suffixArray, suffixArray(fwd), suffixArray(rev) + anchor_length, kmer_d = anchor_d, front_clip = front_clip, end_clip = end_clip) match {
                case Some(x) => matches += x
                case None => return None
              }
            }
          }
        }
      }
    }
    if(matches.nonEmpty) Some(matches.toList) else None
  }

  def find_inversion_site(P : String, d : Int, k : Int, suffixArray : Array[Int], start : Int, end : Int, kmer_d : Int = 1,
                     match_bonus : Int = 1, mismatch_penalty : Int = -1, gap_penalty : Int = -2, report : Int = 4,
                     seed_length : Int = 0, front_clip : Int = 0, end_clip : Int = 0, max_distance : Int = 4,
                          max_anchor_differences : Int = 2, min_length : Int = 10) : Option[ScoredBiInterval] = {
    var differences = 0
    var prev_differences = 0
    var fwd_end = start
    var fwd_done = false
    var rev_end = end
    var rev_done = false
    var P_start = front_clip
    var P_end = P.length - end_clip
    // use kmers to check chain forward and backwards to find inverted region
    breakable {
      for (i <- front_clip until P.length - k - end_clip by 1) { // by k or 1?
        if (!fwd_done) {
          fwd_done = true
          find_approximate_match_staged(P.substring(i, i + k), kmer_d, match_bonus, mismatch_penalty, gap_penalty, report, seed_length) match {
            case Some(x) => {
              //println(s"Matched at i=$i")
              for (ival <- x) {
                for (position <- ival.fwd_l to ival.fwd_r) {
                  if (math.abs(suffixArray(position) - fwd_end) <= max_distance) {
                    fwd_done = false
                    fwd_end = suffixArray(position)
                    prev_differences = ival.differences
                    P_start = i + k
                  }
                }
              }
            }
            case None => fwd_done = true //pass
          }
        }
        if (fwd_done) {
          break
        }
      }
    }
    breakable {
      for (i <- P.length - end_clip until front_clip + k by -1) { // by k or 1?
        if (!rev_done) {
          rev_done = true
          find_approximate_match_staged(P.substring(i - k, i), kmer_d, match_bonus, mismatch_penalty, gap_penalty, report, seed_length) match {
            case Some(x) => {
              //println(s"Matched at i=$i")
              for (ival <- x) {
                for (position <- ival.fwd_l to ival.fwd_r) {
                  if (math.abs(suffixArray(position) + k - rev_end) <= max_distance) {
                    rev_done = false
                    differences += ival.differences
                    rev_end = suffixArray(position) + ival.depth
                    P_end = i - ival.depth
                  }
                }
              }
            }
            case None => rev_done = true //pass
          }
        }
      }
      if (rev_done) {
        break
      }
    }
    //make sure we don't have too many differences
    if(differences > max_anchor_differences){
      return None // fix how differences are counted
    }
    if(P_end - P_start < min_length) {
      return None
    }
    //println(P_start, P_end)
    find_approximate_match_staged(SequenceUtils.reverseComplement(P.substring(P_start, P_end)), d, match_bonus,
      mismatch_penalty, gap_penalty, report, seed_length) match {
      case Some(x) => {
        for(ival <- x) {
          for(position <- ival.fwd_l to ival.fwd_r) {
            if (start <= suffixArray(position) && suffixArray(position) <= end) {
              return Some(new ScoredBiInterval(position, position, -1, -1, ival.depth, ival.score, ival.differences)) //TODO: find reverse interval
            }
          }
        }
      }
      case None => return None
    }
    None
  }

}
