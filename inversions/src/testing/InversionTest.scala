package testing

import indexing.FastaIndex
import io.{FastQReader, SequenceUtils}
import suffixtree.{BWT2, BWTSuffixTree}

import scala.collection.mutable
import scala.io.Source

object InversionTest extends App {

  if (args.length < 2) {
    println("Arguments: 1. index base name and 2. FASTQ file and 3. Mismatches")
    sys.exit(1)
  }

  val decimation_factor = 1
  val alphabet = Map('$' -> 0, 'A' -> 1, 'C' -> 2, 'G' -> 3, 'N' -> 4, 'T' -> 5) //has to be sorted by char
  val fwd_bwt = Source.fromFile(args(0) + ".fbwt").mkString
  val rev_bwt = Source.fromFile(args(0) + ".rbwt").mkString
  val bwt2 = new BWT2(fwd_bwt, rev_bwt, decimation_factor, alphabet)
  val s_tree = new BWTSuffixTree(bwt2)
  val s_arr = (for {l <- Source.fromFile(args(0) + ".fsa").getLines()} yield l.toInt).toArray
  val reader = new FastQReader(args(1))
  val idx_map = new mutable.HashMap[String, Int]()
  for (line <- Source.fromFile(args(0) + ".idx").getLines()) {
    val v = line.split("\t")
    idx_map += (v(0) -> v(1).toInt)
  }
  val fasta_idx = new FastaIndex(idx_map.toMap)


  for (rec <- reader.take(1000)) {
    if(!rec.sequence.contains("N")) {
      rec.trim(20, 20) match {
        case Some(x) => {
          println(s"ID: ${x.id}")
          println(s"Trimmed sequence length: ${x.length()}")
          s_tree.find_inversion(rec.sequence, args(2).toInt, s_arr) match {
            case Some(f) => for (q <- f) for (v <- q.fwd_l to q.fwd_r) println(s"Found inversion at ${v} in BWT (${fasta_idx.getChromosomalPosition(s_arr(v)).get._1} ${fasta_idx.getChromosomalPosition(s_arr(v)).get._2} in text) with ${q.differences} differences with score ${q.score}")
            case None => println("Could not find inversion")
          }
        }
        case None => println("Could not trim")
      }
    }
    println("----------------------------------------------------------------------------------------------------------")
  }

}
