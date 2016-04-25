// fails
// TYPE_NOT_FOUND
// TYPE_NOT_FOUND
// TYPE_NOT_FOUND
// NAME_NOT_FOUND
// TYPE_MISMATCH

package java.util;

// import java.lang.*;
import java.io.File;
import java.io.JFile;  // fails


class A extends Object {
  String fname = "filename";
  File   file  = new java.io.File(fname);
  JFile  jfile  = new JFile(fname); /* fails four times:
                                       1- The first use of JFille
                                       2- The second use of JFille
                                       3- The missing constructor of JFille
                                       4- The unresolved types information
                                     */


  public String m(String a) {
    String s = "";
    return m(s);
  }
}

