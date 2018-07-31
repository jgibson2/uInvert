package indexing

object TernaryQuicksorter {
  /*
  https://www.cs.princeton.edu/~rs/talks/QuicksortIsOptimal.pdf
   */
  val rand = new util.Random()

  def swap(A: Array[Int], a: Integer, b: Integer): Unit = {
    if((a < 0 || a >= A.length) || (b < 0 || b >= A.length)) {
      throw new RuntimeException(s"Unable to swap elements! Index Out of Bounds: ${a} ${b}")
    }
    val temp = A(a)
    A(a) = A(b)
    A(b) = temp
  }

  def sort(A: Array[Int], low: Int, high: Int, G: Array[Int], offset: Int) : Unit = {
    if(low > high){
      return
    }
    if(low == high) {
      G(A(low)) = low
      return
    }

    val pivot : Int = G(A(low + rand.nextInt(high - low + 1)) + offset) //pivot element
    var l = low // counters for index
    var r = high
    var equal_l = low
    var equal_r = high

    // variables for keeping track of partitions in the array
    while(l <= r) {
      // find an l element greater than the pivot
      while(l <= r && G(A(l) + offset) <= pivot) {
        // if the elements are equal to the pivot move them to the sides
        if (G(A(l) + offset) == pivot) {
          swap(A, equal_l, l)
          equal_l += 1
        }
        l += 1
      }
      // find an r element less than the pivot
      while(l <= r && G(A(r) + offset) >= pivot) {
        // if the elements are equal to the pivot move them to the sides
        if (G(A(r) + offset) == pivot) {
          swap(A, equal_r, r)
          equal_r -= 1
        }
        r -= 1
      }
      //check we haven't crossed
      if(l <= r) {
        //swap the elements
        swap(A, r, l)
        l += 1
        r -= 1
      }
    }

    // now we need to swap into the center all the equal elements
    equal_l -= 1 //we went too far initially, so we go back one to be at the last elements assigned
    l -= 1
    while(low <= equal_l) {
      swap(A, l, equal_l)
      equal_l -= 1
      l -= 1
    }
    equal_r += 1 //we went too far initially, so we go back one to be at the last elements assigned
    r += 1
    while(equal_r <= high) {
      swap(A, r, equal_r)
      equal_r += 1 //we went too far initially, so we go back one to be at the last elements assigned
      r += 1
    }


    //recursively sort the lesser section
    sort(A, low, l, G, offset)
    // now we need to update the groups of the equal section
    for(k <- l+1 until r) {
      G(A(k)) = r-1
    }
    if(r-(l+1) == 1) {
      // this group is sorted, and is in its final position
      A(l+1) = -1
    }

    //now sort the greater section
    sort(A, r, high, G, offset)
  }
}
