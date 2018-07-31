package testing

import io.FastQReader

object FastQReadTest extends App {

  if(args.length < 1) {
    println("Arguments: 1. file name")
    sys.exit(1)
  }

  val reader = new FastQReader(args(0))
  var i = 0
  for(rec <- reader) {
    //println(rec.sequence, rec.qualities)
    rec.trim(20, 20) match {
      case Some(x) => i += 1
      case None => println("Count not trim record")
    }

  }
  println(s"${i} records trimmed in file")
}
