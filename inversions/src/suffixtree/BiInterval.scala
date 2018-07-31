package suffixtree

class BiInterval(val fwd_l : Int, val fwd_r : Int, val rev_l : Int, val rev_r : Int, val depth : Int) extends Ordered[BiInterval] {
  def compare(that : BiInterval) : Int = (this.fwd_r - this.fwd_l).compare(that.fwd_r - that.fwd_l)

  def length() : Int = fwd_r - fwd_l + 1
}
