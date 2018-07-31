package indexing

object SuffixArrayBuilder {

  def buildSuffixArray(s: String) : Array[Int] = {
    //build array
    var A: Array[Int] = new Array[Int](s.length)
    for(i <- A.indices) {
      A(i) = i
    }
    //sort by first char

    def sortByChar(a : Int, b: Int) = {
      s.charAt(a) < s.charAt(b)
    }

    A = A.sortWith(sortByChar)
    //group array
    val G: Array[Int] = Array.fill[Int](s.length)(0)
    // val L: Array[Integer] = Array.fill[Integer](s.length)(0)

    //update G
    initializeGroupAndLengthArray(A, G, s)

    var d = 1


    var unsorted = true
    while(unsorted) { //while the array is not sorted
      unsorted = false
      var i : Int = 0
      var num_sorted = 0
      while(i < A.length) {   //work through the length array
        if (A(i) >= 0 ) {
          if(num_sorted != 0) {
            //num_sorted will be negative
            A(i + num_sorted) = num_sorted
            num_sorted = 0
          }
          val tmp_i = G(A(i)) + 1 //because we're changing i
          if(i < G(A(i))) {
            TernaryQuicksorter.sort(A, i, G(A(i)), G, d)
            unsorted = true
          }
          i = tmp_i
        } else {
          num_sorted += A(i)
          i -= A(i)
        }
      }
      if(num_sorted != 0) {
        A(i + num_sorted) = num_sorted
      }
      d *= 2 //double d
    }
    for(i <- A.indices) {
      A(G(i)) = i
    }

    A //return A
  }


  def initializeGroupAndLengthArray(A: Array[Int], G: Array[Int], s: String): Unit = {
    var pos = 0 //position in string
    var start = 0 //start of group
    var prev = s.charAt(A(0)) //previous suffix's group value
    var g = 0 //group number
    while(pos != A.length) {
      if(s.charAt(A(pos)) == prev) {
        //if group is the same as the previous, update new group number
        g = pos
      }
      else {
        // now that we're out of the group, assign the group numbers
        for(i <- start until pos) {
          G(A(i)) = g
        }

        //reset all the numbers for the next iteration
        start = pos
        g = pos
        prev = s.charAt(A(pos))
      }
      pos += 1 //move forward in the string
    }
    for(i <- start until pos) {
      G(A(i)) = g
    }


  }
}
