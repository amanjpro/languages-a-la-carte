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

trait PackageDefApi extends NamedTree with SymTree {
  def members: List[DefTree]
  val tpe: Option[Type] = None
}


trait ClassDefApi extends TypeTree {
  def mods: Flags
  def name: Name
  def parents: List[UseTree]
  def body: TemplateApi

  val tpe: Option[Type] = symbol.flatMap(_.tpe)
  // {
  //   val ptpes = parents.flatMap(_.tpe).toSet
  //   // Is it Object? (No java.lang.Object is defined at this module level)
  //   // This needs to be in typers not here
  //   if(name == OBJECT_TYPE_NAME) {
  //     Some(ObjectType)
  //   } else Some(ClassType(name, ptpes))
  // }
}


trait TemplateApi extends Tree {
  def members: List[Tree]
  val tpe: Option[Type] = None
}

trait NewApi extends Expr {
  def tpt: UseTree
  def args: List[Expr]
  val tpe: Option[Type] = tpt.tpe
}

trait SelectApi extends UseTree with Expr {
  def qual: Tree
  def tree: SimpleUseTree

  // override val name: ContextState[Name] = tree.name
  def uses: Option[Symbol] = tree.symbol
  def name: Name           = tree.name
}

trait ThisApi extends Expr {
  def enclosingClassSymbol: Option[Symbol]

  val tpe: Option[Type] = enclosingClassSymbol.flatMap(_.tpe)
}

trait SuperApi extends Expr {
  def enclosingClassSymbol: Option[Symbol]

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
case class PackageDef(members: List[DefTree], tpe: Option[Type],
  pos: Option[Position], symbol: Option[Symbol], owner: Option[Symbol])

case class ClassDef(mods: Flags, name: Name, parents: List[UseTree],
  body: TemplateApi, pos: Option[Position], symbol: Option[Symbol],
  owner: Option[Symbol]) extends ClassDefApi

case class Tempalte(members: List[Tree], pos: Option[Position], owner:
  Option[Symbol]) extends TemplateApi
