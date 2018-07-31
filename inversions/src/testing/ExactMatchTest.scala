package testing

import suffixtree.{BWT2, BWTSuffixTree}

import scala.io.Source

object ExactMatchTest extends App {

  if(args.length < 2) {
    println("Arguments: 1. index base name and 2. pattern")
    sys.exit(1)
  }

  val decimation_factor = 32
  val alphabet = Map('$' -> 0, 'A' -> 1, 'C' -> 2, 'G' -> 3, 'N' -> 4, 'T' -> 5) //has to be sorted by char
  val fwd_bwt = Source.fromFile(args(0) + ".fbwt").mkString
  val rev_bwt = Source.fromFile(args(0) + ".rbwt").mkString
  val bwt2 = new BWT2(fwd_bwt, rev_bwt, decimation_factor, alphabet)
  val s_tree = new BWTSuffixTree(bwt2)
  val s_arr = (for{l <- Source.fromFile(args(0) + ".fsa").getLines()} yield l.toInt).toArray
  val pat = args(1)

  println(s_tree.find_exact_match(pat) match {
    case Some(i) => s"Found exact match at ${i.fwd_l} in BWT (${s_arr(i.fwd_l)} in text)"
    case None => s"No match"
  })

}
