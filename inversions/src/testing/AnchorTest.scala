package testing

import indexing.FastaIndex
import io.{FastQReader, SequenceUtils}
import suffixtree.{BWT2, BWTSuffixTree}

import scala.collection.mutable
import scala.io.Source

object AnchorTest extends App {

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

//  for (rec <- reader.take(1000)) {
//    var found = true
//    rec.trim(20, 20) match {
//      case Some(x) => {
//        println(s"ID: ${x.id}")
//        println(s"Trimmed sequence length: ${x.length()}")
//        s_tree.find_approximate_match_staged(x.sequence, args(2).toInt) match {
//          case Some(i) => {
//            for (q <- i) for (v <- q.fwd_l to q.fwd_r) println(s"Found inexact match at ${v} in BWT (${fasta_idx.getChromosomalPosition(s_arr(v)).get._1} ${fasta_idx.getChromosomalPosition(s_arr(v)).get._2} in text) with ${q.differences} differences with score ${q.score}")
//          }
//          case None => {
//            found = false
//            s_tree.find_forward_anchor(x.sequence, 1, clip = 0) match {
//              case Some(f) => for (q <- f) for (v <- q.fwd_l to q.fwd_r) println(s"Found forward anchor match at ${v} in BWT (${fasta_idx.getChromosomalPosition(s_arr(v)).get._1} ${fasta_idx.getChromosomalPosition(s_arr(v)).get._2} in text) with ${q.differences} differences with score ${q.score}")
//              case None => println("Could not find forward anchor")
//            }
//            s_tree.find_reverse_anchor(x.sequence, 1, clip = 0) match {
//              case Some(f) => for (q <- f) for (v <- q.fwd_l to q.fwd_r) println(s"Found reverse anchor match at ${v} in BWT (${fasta_idx.getChromosomalPosition(s_arr(v)).get._1} ${fasta_idx.getChromosomalPosition(s_arr(v)).get._2} in text) with ${q.differences} differences with score ${q.score}")
//              case None => println("Could not find reverse anchor")
//            }
//          }
//        }
//        s_tree.find_approximate_match_staged(SequenceUtils.reverseComplement(x.sequence), args(2).toInt) match {
//          case Some(i) => {
//            for (q <- i) for (v <- q.fwd_l to q.fwd_r) println(s"Found reverse-complemented inexact match at ${v} in BWT (${fasta_idx.getChromosomalPosition(s_arr(v)).get._1} ${fasta_idx.getChromosomalPosition(s_arr(v)).get._2} in text) with ${q.differences} differences with score ${q.score}")
//          }
//          case None => {
//            if(!found) println("No match")
//            s_tree.find_forward_anchor(SequenceUtils.reverseComplement(x.sequence), 1, clip = 0) match {
//              case Some(f) => for (q <- f) for (v <- q.fwd_l to q.fwd_r) println(s"Found reverse-complemented forward anchor match at ${v} in BWT (${fasta_idx.getChromosomalPosition(s_arr(v)).get._1} ${fasta_idx.getChromosomalPosition(s_arr(v)).get._2} in text) with ${q.differences} differences with score ${q.score}")
//              case None => println("Could not find forward anchor")
//            }
//            s_tree.find_reverse_anchor(SequenceUtils.reverseComplement(x.sequence), 1, clip = 0) match {
//              case Some(f) => for (q <- f) for (v <- q.fwd_l to q.fwd_r) println(s"Found reverse-complemented reverse anchor match at ${v} in BWT (${fasta_idx.getChromosomalPosition(s_arr(v)).get._1} ${fasta_idx.getChromosomalPosition(s_arr(v)).get._2} in text) with ${q.differences} differences with score ${q.score}")
//              case None => println("Could not find reverse anchor")
//            }
//          }
//        }
//      }
//      case None => println("Could not trim")
//    }
//    println("----------------------------------------------------------------------------------------------------------")
//  }

  for (rec <- reader.take(1000)) {
    rec.trim(20, 20) match {
      case Some(x) => {
        println(s"ID: ${x.id}")
        println(s"Trimmed sequence length: ${x.length()}")
        s_tree.find_forward_anchor(x.sequence, 1, clip = 0) match {
          case Some(f) => for (q <- f) for (v <- q.fwd_l to q.fwd_r) println(s"Found forward anchor match at ${v} in BWT (${fasta_idx.getChromosomalPosition(s_arr(v)).get._1} ${fasta_idx.getChromosomalPosition(s_arr(v)).get._2} in text) with ${q.differences} differences with score ${q.score}")
          case None => println("Could not find forward anchor")
        }
        s_tree.find_reverse_anchor(x.sequence, 1, clip = 0) match {
          case Some(f) => for (q <- f) for (v <- q.fwd_l to q.fwd_r) println(s"Found reverse anchor match at ${v} in BWT (${fasta_idx.getChromosomalPosition(s_arr(v)).get._1} ${fasta_idx.getChromosomalPosition(s_arr(v)).get._2} in text) with ${q.differences} differences with score ${q.score}")
          case None => println("Could not find reverse anchor")
        }
//        s_tree.find_exact_forward_anchor(x.sequence) match {
//          case Some(f) => for (v <- f.fwd_l to f.fwd_r) println(s"Found forward anchor match at ${v} in BWT (${fasta_idx.getChromosomalPosition(s_arr(v)).get._1} ${fasta_idx.getChromosomalPosition(s_arr(v)).get._2} in text)")
//          case None => println("Could not find forward anchor")
//        }
//        s_tree.find_exact_reverse_anchor(x.sequence) match {
//          case Some(f) => for (v <- f.fwd_l to f.fwd_r) println(s"Found reverse anchor match at ${v} in BWT (${fasta_idx.getChromosomalPosition(s_arr(v)).get._1} ${fasta_idx.getChromosomalPosition(s_arr(v)).get._2} in text)")
//          case None => println("Could not find reverse anchor")
//        }
      }
      case None => println("Could not trim")
    }
    println("----------------------------------------------------------------------------------------------------------")
  }

}
