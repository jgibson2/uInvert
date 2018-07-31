package suffixtree

class ScoredBiInterval(val fwd_l : Int, val fwd_r : Int, val rev_l : Int, val rev_r : Int, val depth : Int, val score : Int, val differences : Int) extends Ordered[ScoredBiInterval] {

  def compare(that : ScoredBiInterval) : Int = this.score.compare(that.score)
  def length() : Int = fwd_r - fwd_l + 1
  def toBiInterval : BiInterval = new BiInterval(fwd_l, fwd_l, rev_l, rev_r, depth)
}
