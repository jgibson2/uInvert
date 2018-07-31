package indexing

import java.io.{File, FileReader, FileWriter}


import scala.collection.mutable.ListBuffer
import scala.io
import _root_.io.FastaReader

object Indexer extends App {

  if(args.length < 2) {
    println("Arguments: 1. text file and 2. output filename base!")
    sys.exit(1)
  }

  val reader = new FastaReader(args(0))
  val s = reader.sequence + "$"
  val sr = reader.sequence.reverse + "$"

  val a = SuffixArrayBuilder.buildSuffixArray(s)
  val ar = SuffixArrayBuilder.buildSuffixArray(sr)

  val bwt : String =  {
    val l = new ListBuffer[Char]
    for(i <- a) {
      if (i==0) {
        l += '$'
      } else {
        l += s.charAt(i-1)
      }
    }
    l.mkString("")
  }

  val bwt_rev : String =  {
    val l = new ListBuffer[Char]
    for(i <- ar) {
      if (i==0) {
        l += '$'
      } else {
        l += sr.charAt(i-1)
      }
    }
    l.mkString("")
  }

  val fasta_idx = (for(s <- reader.nameMap.keys.toStream) yield s"${s}\t${reader.nameMap(s)}").mkString("\n")


  val fname_fwd_bwt = args(1) +     ".fbwt"
  val fname_rev_bwt = args(1) +     ".rbwt"
  val fname_suffix_arr = args(1) +  ".fsa"
  val fname_fasta_index = args(1) + ".idx"

  val f = new File(fname_fwd_bwt)
  val fs = new FileWriter(f)
  fs.write(bwt)
  fs.flush()
  fs.close()

  val fr = new File(fname_rev_bwt)
  val fsr = new FileWriter(fr)
  fsr.write(bwt_rev)
  fsr.flush()
  fsr.close()

  val fa = new File(fname_suffix_arr)
  val far = new FileWriter(fa)
  far.write(a.mkString("\n"))
  far.flush()
  far.close()

  val fi = new File(fname_fasta_index)
  val fir = new FileWriter(fi)
  fir.write(fasta_idx)
  fir.flush()
  fir.close()

}
