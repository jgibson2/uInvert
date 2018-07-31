package testing

import suffixtree.{BWT2, BWTSuffixTree}

import scala.io.Source

object InexactMatchTest extends App {

  if(args.length < 2) {
    println("Arguments: 1. index base name and 2. pattern")
    sys.exit(1)
  }

  val decimation_factor = 32
  val alphabet = Map('$' -> 0, 'A' -> 1, 'C' -> 2, 'G' -> 3, 'T' -> 4)
  val fwd_bwt = Source.fromFile(args(0) + ".fbwt").mkString
  val rev_bwt = Source.fromFile(args(0) + ".rbwt").mkString
  val bwt2 = new BWT2(fwd_bwt, rev_bwt, decimation_factor, alphabet)
  val s_tree = new BWTSuffixTree(bwt2)
  val s_arr = (for{l <- Source.fromFile(args(0) + ".fsa").getLines()} yield l.toInt).toArray
  val pat = args(1)

  s_tree.find_approximate_match(pat, args(2).toInt) match {
    case Some(i) => {
      for(x <- i) {
        for(v <- x.fwd_l to x.fwd_r) println(s"\nFound inexact match at ${v} in BWT (${s_arr(v)} in text) with ${x.differences} differences with score ${x.score}")
      }
    }
    case None => println(s"\nNo match")
  }

}
