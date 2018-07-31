package testing

import indexing.FastaIndex
import io.{FastQReader, SequenceUtils}
import suffixtree.{BWT2, BWTSuffixTree}

import scala.collection.mutable
import scala.io.Source

object FullAlignmentTest extends App {

  if (args.length < 2) {
    println("Arguments: 1. index base name and 2. FASTQ file and 3. Mismatches")
    sys.exit(1)
  }

  val decimation_factor = 1
  val alphabet = Map('$' -> 0, 'A' -> 1, 'C' -> 2, 'G' -> 3, 'T' -> 4, 'N' -> 5)
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

//  for (rec <- reader.take(1000).toStream.par) {
//    rec.trim(20, 20) match {
//      case Some(x) => {
//        s_tree.find_exact_match(x.sequence) match {
//          case Some(q) => for (v <- q.fwd_l to q.fwd_r) println(s"\nFound exact match at ${v} in BWT (${fasta_idx.getChromosomalPosition(s_arr(v)).get._1} ${fasta_idx.getChromosomalPosition(s_arr(v)).get._2} in text)")
//          case None => s_tree.find_approximate_ungapped_match(x.sequence, args(2).toInt) match {
//            case Some(i) => for (q <- i) for (v <- q.fwd_l to q.fwd_r) println(s"\nFound ungapped inexact match at ${v} in BWT (${fasta_idx.getChromosomalPosition(s_arr(v)).get._1} ${fasta_idx.getChromosomalPosition(s_arr(v)).get._2} in text) with ${q.differences} differences with score ${q.score}")
//            case None => s_tree.find_approximate_match(x.sequence, args(2).toInt) match {
//              case Some(i) => for (q <- i) for (v <- q.fwd_l to q.fwd_r) println(s"\nFound inexact match at ${v} in BWT (${fasta_idx.getChromosomalPosition(s_arr(v)).get._1} ${fasta_idx.getChromosomalPosition(s_arr(v)).get._2} in text) with ${q.differences} differences with score ${q.score}")
//              case None => println(s"\nNo match")
//            }
//          }
//        }
//      }
//      case None => println("Could not trim")
//    }
//  }

  for (rec <- reader.take(1000).toStream.par) {
    var found = true
    rec.trim(20, 20) match {
      case Some(x) => {
        s_tree.find_approximate_match_staged(x.sequence, args(2).toInt) match {
          case Some(i) => for (q <- i) for (v <- q.fwd_l to q.fwd_r) println(s"Found inexact match at ${v} in BWT (${fasta_idx.getChromosomalPosition(s_arr(v)).get._1} ${fasta_idx.getChromosomalPosition(s_arr(v)).get._2} in text) with ${q.differences} differences with score ${q.score}")
          case None => found = false
        }
        s_tree.find_approximate_match_staged(SequenceUtils.reverseComplement(x.sequence), args(2).toInt) match {
          case Some(i) => for (q <- i) for (v <- q.fwd_l to q.fwd_r) println(s"Found reverse-complemented inexact match at ${v} in BWT (${fasta_idx.getChromosomalPosition(s_arr(v)).get._1} ${fasta_idx.getChromosomalPosition(s_arr(v)).get._2} in text) with ${q.differences} differences with score ${q.score}")
          case None => if(!found) println("No match")
        }
      }
      case None => println("Could not trim")
    }
  }

}
