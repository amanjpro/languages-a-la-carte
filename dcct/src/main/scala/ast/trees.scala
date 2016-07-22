/*
 * Copyright (c) <2015-2016>, see CONTRIBUTORS
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

package ch.usi.inf.l3.sana.dcct.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import sana.ooj
import sana.calcj
import tiny.ast._
import ooj.ast._
import calcj.ast._
import primj.ast._
import tiny.source.Position
import tiny.types.Type
import tiny.names.Name
import tiny.symbols.Symbol
import tiny.modifiers.Flags
import primj.types._
/**
 * Some things to keep in mind when defining trees:
 * 1) This framework is all about reusing trees from other modules as much as possible.
 * 2) As far as I have understood, no need to be too type specific or type safe when defining
 * trees, so we do not lose flexibility (not convinced). If needed, check the shapes of trees
 * at a compilation phase. Let the parser rule out wrong shapes, and handle semantics at
 * code generation.
 * 3) How to know whether to name a tree as x or xApi? and xApi is a tree that is meant to be instantiated.
 * while x is not. Note however, that you are supposed to provide factories to instantiate trees.
 * 4) How to know whether to create concrete tree fields, or open-class style fields? generally,
 * things that have to do with the actual shape of the tree are concrete fiendsl, other things like the
 * symbol are an open-class style field. Again notice that you are never supposed to manually manipulate the map.
 * but you have to do it throught implicit style conversions.
 *
 */
/**
 * This trait is used to restrict possible DefTrees that
 * can be a part of a Dcct program.
 *
 * TODO might want to remove it later and just use DefTree, relying on my parser to generate
 * the correct trees used only by my language. Actually I think I will try that now!
 */
//trait DcctDefTree extends DefTree {
//}

/**
 * Program
 * The root of my compilation tree. Will take from primj
 * a program starts with schema delarations, that is, entities
 * and arrays, then a list of functions that operate on the schema.
 *
 * Example:
 *
 * // All schema declarations at the beginning of the program, used to
 * create a data store schema.
 * entity Customer {
 *   address: CString
 * }
 *
 * // functions that operate on the schema.
 * function createCusomer() {
 *  ...
 * }
 *
 * Used tree: sana.primj.ast.ProgramApi
 */


/**
 * Repesents an entity definition of a scheme. Entities appear at the beginning of the program.
 * propoerties are attached to entities, and point only to cloud types. In this language, each
 * property is at an integer index.
 * examples:
 *
 * entity Customer {
 *   name: CString // propoerty, this is syntax sugar.
 * }
 *
 * entity Order(customer: Customer) { // this is a weak entity, depends of customer.
 *   time: CTime
 * }
 *
 * an entity is like a class, except that it does not contain methods, hence we use the class
 * from ooj to utilize constructors and similar features.
 *
 * TODO I cannot find the constructor in ClassDef!
 * I believe it has to be added during compilation, after assigning a symbol or something.
 *
 *
 */

/**
 * Arrays are special entities, they always exist with some default value, and have
 * explicit indices. Their semantics eliminate contention on indices.
 *
 * In theory, I could use the class definition trees to implement them, but it seems too awkward
 * new and stuff will not work. So I will just define a new tree for them.
 *
 * TODO consult Amanj about using the ClassDef tree for arrays.
 */
trait ArrayDefApi extends DefTree {
  def name: Name
  def indices: List[ValDefApi]
  def properties: List[ValDefApi]

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = indices.foldLeft(z)((z, y) => {
      y.bottomUp(z)(f)
    })
    val r2 = properties.foldLeft(z)((z, y) => {
      y.bottomUp(z)(f)
    })

    f(r2, this) // TODO not sure if r2 here is correct.
  }
}

/**
 * TODO how to validate declared types? how to attach tpt to trees?
 * Binding happens at parser and validation at typer. Look at examples from primJ
 */

/**
 * Here I scanned my grammar and tried to figure out what to do with all language components.
 *
 * 1) Cloud, index and expression types assigned at parser, checked at typer and exist
 * at the runtime, in the future....
 *
 * 2) I do not see the point of having this weird property sugar and de-sugar. I think it simplifies the grammar
 * but nothing more. I will just have them as as part of my trees I think.
 *
 * 3) The update and query operatiosn are just applies, and are a part of the runtimes. Maybe I provide
 * language constructs in the future to actually define them.
 *
 * 4) delete, entries and all are treated as apply.
 *
 */

/**
 * This construct is used to query entities and lists.
 * Example:
 *
 * foreach order in all Order
 * where order.Customer == customer
 * { 'do something here' }
 *
 */
trait ForEachApi extends Expr {
  /** the entity variable */
  def entityVar: ValDefApi
  /** the `where` clause */
  def whereExpr: Expr
  /** the body of the loop */
  def body: BlockApi

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = entityVar.bottomUp(z)(f)
    val r2 = whereExpr.bottomUp(r1)(f)
    val r3 = body.bottomUp(r2)(f)

    f(r3, this)
  }
}

/**
 * entity and array filters are 'all' and 'entries'. I will implement them
 * using Apply from primj.
 *
 * TODO I am not sure if I can use methods with apply for everything. I could have some sort of Env
 * object and call methods on that. I do not really like it though but whatever.
 */

/**
 * 'all' returns a list of all instances of an entity
 *
 */

/**
 * 'entries p' returns a subset of elements in an array, where
 * 'p' is set to a value other than the default.
 */

/**
 * flush and yeild are also implemented using Apply!
 */

protected[ast] class ForEach(val entityVar: ValDefApi,val whereExpr: Expr, val body: BlockApi)
extends ForEachApi {
  override def toString: String =
    s"ForEach($entityVar, $whereExpr, $body)"
}

protected[ast] class ArrayDef(val name: Name, val indices: List[ValDefApi], val properties: List[ValDefApi]) extends ArrayDefApi {
  override def toString: String =
    s"Array$indices,$properties)"
}
