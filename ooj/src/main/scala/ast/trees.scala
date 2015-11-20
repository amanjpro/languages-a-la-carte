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

trait CompilationUnitApi extends Tree {
  def module: PackageDefApi
  def sourceName: String
  // the head of the list contains the inner most directory
  def sourcePath: List[String]
}

trait PackageDefApi extends NamedTree {
  def members: List[Tree]
}


trait ClassDefApi extends TypeTree {
  def mods: Flags
  def name: Name
  def parents: List[UseTree]
  def body: TemplateApi

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
}

trait MethodDefApi extends primj.ast.MethodDefApi {
  def mods: Flags
}

trait NewApi extends Expr {
  def tpt: UseTree
  def args: List[Expr]
}

trait SelectApi extends UseTree with Expr {
  def qual: Tree
  def tree: SimpleUseTree

  // override val name: ContextState[Name] = tree.name
  // def uses: Option[Symbol] = tree.symbol
  def name: Name           = tree.name
}

trait ThisApi extends Expr {
  // def enclosingClassSymbol: Option[Symbol]

}

trait SuperApi extends Expr {
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
  val sourceName: String, val sourcePath: List[String]) extends CompilationUnitApi

protected[ast] class PackageDef(val name: Name,
  val members: List[Tree]) extends PackageDefApi

protected[ast] class ClassDef(val mods: Flags,
  val name: Name, val parents: List[UseTree],
  val body: TemplateApi) extends ClassDefApi

protected[ast] class Template(val members: List[Tree]) extends TemplateApi

protected[ast] class New(val tpt: UseTree, val args: List[Expr]) extends NewApi
protected[ast] class Select(val qual: Tree,
  val tree: SimpleUseTree) extends SelectApi
protected[ast] class This() extends ThisApi
protected[ast] class Super() extends SuperApi


protected[ast] class MethodDef(val mods: Flags,
  val ret: UseTree,
  val name: Name, val params: List[ValDefApi],
  val body: Expr) extends MethodDefApi
