// fails
// TYPE_MISMATCH


class Arrays {
  int array[][] = new int[3][];
  int array2[]  = {1, 2, 3, 4};
  int array3[]  = {1, "klj", 3, 4}; // fails


  void m() {
    int k = array[2][3];
    k     = array2[3];
  }
}

