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


  Object b1 = new Object();
  Object b2 = new Object();
  Object b3 = new Object();
  public String m(String a) {
    String s = "";
    Object b4 = new Object();
    Object b5 = new Object();
    return m(s);
  }
}

