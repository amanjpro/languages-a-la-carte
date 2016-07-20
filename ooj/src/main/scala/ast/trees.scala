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

package ch.usi.inf.l3.sana.ooj.ast


import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.brokenj
import sana.ooj

import tiny.source.Position
import tiny.names.Name
import tiny.symbols.Symbol
import tiny.modifiers.Flags

import tiny.ast._
import calcj.ast._
import primj.ast.{Program => _, _}
import brokenj.ast._

import tiny.types.Type
import calcj.types._
import primj.types._
import ooj.types._

import ooj.names.StdNames._



/********************* AST Nodes *********************************/

/** A tree to represent programs in ooj. */
trait ProgramApi extends Tree {
  /** A list of members of a program */
  def members: List[Tree]
  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = members.foldLeft(z)((z, y) => {
      y.bottomUp(z)(f)
    })
    f(r1, this)
  }
}

/**
 * A tree to represent a compilation unit. In Java a compilation unit is
 * a single source file.
 */
trait CompilationUnitApi extends Tree {
  /** The package that is in this compilation unit */
  def module: PackageDefApi
  /** The name of the source file that this compilation unit represents */
  def sourceName: String
  /**
   * The path of the source file that this compilation unit represents.
   * The head of the list contains the inner most directory
   */
  def sourcePath: List[String]

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = module.bottomUp(z)(f)
    f(r1, this)
  }
}

/** A tree to represent a package */
trait PackageDefApi extends TermTree {
  /** A list of members of this package */
  def members: List[Tree]

  /** The name of this package */
  def name: Name
  /**
   * A list of the package that contains it, if the package is `pkg1.pkg2.pkg3.thisPkg`.
   * Then the list will be: List(pkg1, pkg2, pkg3) (hence, excluding this packages name).
   * The head of the list contains the outer most package.
   */
  def containingPackages: List[Name]

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = members.foldLeft(z)((z, y) => {
      y.bottomUp(z)(f)
    })
    f(r1, this)
  }
}


/** A trait to represent classes and interfaces */
trait ClassDefApi extends TypeTree {
  /** The modifiers (flags) of this tree */
  def mods: Flags
  /** The name of this class/interface */
  def name: Name
  /** A list of parents of this class/interface */
  def parents: List[UseTree]
  /** The body of this class/interface */
  def body: TemplateApi


  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = parents.foldLeft(z)((z, y) => {
      y.bottomUp(z)(f)
    })
    val r2 = body.bottomUp(r1)(f)
    f(r2, this)
  }
  // {
  //   val ptpes = parents.flatMap(_.tpe).toSet
  //   // Is it Object? (No java.lang.Object is defined at this module level)
  //   // This needs to be in typers not here
  //   if(name == OBJECT_TYPE_NAME) {
  //     Some(ObjectType)
  //   } else Some(ClassType(name, ptpes))
  // }
}


/** A tree to represent the body of a class */
trait TemplateApi extends Tree {
  /** The members of the class that this body is part of */
  def members: List[Tree]

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = members.foldLeft(z)((z, y) => {
      y.bottomUp(z)(f)
    })
    f(r1, this)
  }
}

/** A trait to represent a method definition */
trait MethodDefApi extends primj.ast.MethodDefApi {
  /** The modifiers (flags) of this tree */
  def mods: Flags
}

/** A trait to represent `new` expressions */
trait NewApi extends Expr {
  /**
   * The application part of the new. The expression: {{{new A()}}} translates to:
   * {{{
   * new NewApi {
   *   new ApplyApi {
   *     def fun = new SelectApi {
   *       def qual = new TypeUseApi { def name = Name("A") }
   *       def tree = new IdentApi { def name = Name("<init>") }
   *     }
   *     def args = Nil
   *   }
   * }
   * }}}
   */
  def app: ApplyApi

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = app.bottomUp(z)(f)
    f(r1, this)
  }
}

/** A trait to represent member selection like: {{{qual.name}}} */
trait SelectApi extends UseTree with Expr {
  /** The tree which its tree is selected */
  def qual: Tree
  /** The selected name */
  def tree: SimpleUseTree

  // override val name: ContextState[Name] = tree.name
  // def uses: Option[Symbol] = tree.symbol
  /** The name of this tree, equivalent of the {{{this.tree.name}}} */
  def name: Name           = tree.name

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = qual.bottomUp(z)(f)
    val r2 = tree.bottomUp(r1)(f)
    f(r2, this)
  }
}

/** A trait to represent `this` */
trait ThisApi extends Expr {
  // def enclosingClassSymbol: Option[Symbol]
  def bottomUp[R](z: R)(f: (R, Tree) => R): R =
    f(z, this)
}

/** A trait to represent `super` */
trait SuperApi extends Expr {
  def bottomUp[R](z: R)(f: (R, Tree) => R): R =
    f(z, this)
  // def enclosingClassSymbol: Option[Symbol]

  // val tpe: TypeState[Type] = for {
  //   ctx       <- get
  //   ptpe      <- ctx.getTree(owner) match {
  //     case Some(t)          =>
  //       val ty = t.tpe.eval(ctx)
  //       ty match {
  //         case ct: ClassType     =>
  //           val pids = ct.parents.foldLeft(Nil: List[TreeId])((z, y) => {
  //             y match {
  //               case ct: ClassType       => (ct.id)::z
  //               case _                   => z
  //             }
  //           })
  //           pids.filter(ctx.isInterface(_)) match {
  //             case List(x)       => ctx.getTpe(x)
  //             case _             => None
  //           }
  //         case _                 => None
  //       }
  //     case None             => None
  //   }
  // } yield ptpe
}


// case class ClassDef() extends ClassDefApi
protected[ast] class CompilationUnit(val module: PackageDefApi,
  val sourceName: String,
  val sourcePath: List[String]) extends CompilationUnitApi {
  override def toString: String =
    s"CompilationUnit($module, $sourceName, $sourcePath)"
}
protected[ast] class Program(val members: List[Tree])
            extends ProgramApi {
  override def toString: String = s"Program($members)"
}
protected[ast] class PackageDef(val containingPackages: List[Name],
  val name: Name, val members: List[Tree]) extends PackageDefApi {
  override def toString: String =
    s"PackageDef($name, $members)"
}

protected[ast] class ClassDef(val mods: Flags,
  val name: Name, val parents: List[UseTree],
  val body: TemplateApi) extends ClassDefApi {
  override def toString: String =
    s"ClassDef($mods, $name, $parents, $body)"
}

protected[ast] class Template(val members: List[Tree]) extends TemplateApi {
  override def toString: String =
    s"Template($members)"
}

protected[ast] class New(val app: ApplyApi) extends NewApi {
  override def toString: String =
    s"New($app)"
}
protected[ast] class Select(val qual: Tree,
  val tree: SimpleUseTree) extends SelectApi {
  override def toString: String =
    s"Select($qual, $tree)"
}
protected[ast] class This() extends ThisApi {
  override def toString: String =
    s"This"
}
protected[ast] class Super() extends SuperApi {
  override def toString: String =
    s"Super"
}


protected[ast] class MethodDef(val mods: Flags,
  val ret: UseTree,
  val name: Name, val params: List[ValDefApi],
  val body: Expr) extends MethodDefApi {
  override def toString: String =
    s"MethodDef($mods, $ret, $name, $params, $body)"
}
