/*
 * Copyright (c) <2015-2016>, see CONTRIBUTERS
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of the <organization> nor the
 *     names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

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
    * Assigns symbols to the trees, uses symbol assigner.
    *
    * The default implementation calls typeCheck, make sure to adapt
    * it to your needs.
    *
    * @param owner the contextual owner of this tree
    * @param tree the tree to be named
    */
   def resolveNames(owner: Option[Symbol])(tree: Tree): Tree =
     typeCheck(owner)(tree)


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
