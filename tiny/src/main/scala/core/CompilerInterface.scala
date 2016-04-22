package ch.usi.inf.l3.sana.tiny.core


import ch.usi.inf.l3.sana.tiny.ast.Tree
import ch.usi.inf.l3.sana.tiny.symbols.Symbol


trait CompilerInterface {

  /**
    * Type-checks a tree in the given context.
    *
    * This method does everything necessary to type-check a tree.
    * It assigns symbols, resolves the names and type-checks the
    * tree and if necessary reports errors.
    *
    * @param owner the contextual owner of this tree
    * @param tree the tree to be type-checked
    */
  def typeCheck(owner: Option[Symbol])(tree: Tree): Tree


  /**
    * Checks if the given classpath has a module.
    *
    * @param module the fully qualified name of the module
    */
  def definesModule(module: String): Boolean

  /**
    * Loads a binary file (class in JVM) and resolves its
    * type information and names. This method only loads
    * the classfile if it is not already loaded.
    *
    * @param fname the fully qualified name of the class.
    */
  def load(fname: String): Option[Tree]

  /**
    * Parses a string to a Tree
    *
    * @param source the string to be parsed
    */
  def parse(source: String): Tree

  /**
    * Unparses a tree to a String
    *
    * @param tree the tree to be unparsed
    */
  def unparse(tree: Tree): String
}
