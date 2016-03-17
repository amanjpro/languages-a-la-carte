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

trait ProgramApi extends Tree {
  def members: List[Tree]
  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = members.foldLeft(z)((z, y) => {
      y.bottomUp(z)(f)
    })
    f(r1, this)
  }
}

trait CompilationUnitApi extends Tree {
  def module: PackageDefApi
  def sourceName: String
  // the head of the list contains the inner most directory
  def sourcePath: List[String]

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = module.bottomUp(z)(f)
    f(r1, this)
  }
}

trait PackageDefApi extends TermTree {
  def members: List[Tree]

  def name: Name
  // the head of the list contains the outer most package
  def containingPackages: List[Name]

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = members.foldLeft(z)((z, y) => {
      y.bottomUp(z)(f)
    })
    f(r1, this)
  }
}


trait ClassDefApi extends TypeTree {
  def mods: Flags
  def name: Name
  def parents: List[UseTree]
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


trait TemplateApi extends Tree {
  def members: List[Tree]

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = members.foldLeft(z)((z, y) => {
      y.bottomUp(z)(f)
    })
    f(r1, this)
  }
}

trait MethodDefApi extends primj.ast.MethodDefApi {
  def mods: Flags
}

trait NewApi extends Expr {
  def app: ApplyApi

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = app.bottomUp(z)(f)
    f(r1, this)
  }
}

trait SelectApi extends UseTree with Expr {
  def qual: Tree
  def tree: SimpleUseTree

  // override val name: ContextState[Name] = tree.name
  // def uses: Option[Symbol] = tree.symbol
  def name: Name           = tree.name

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = qual.bottomUp(z)(f)
    val r2 = tree.bottomUp(r1)(f)
    f(r2, this)
  }
}

trait ThisApi extends Expr {
  // def enclosingClassSymbol: Option[Symbol]
  def bottomUp[R](z: R)(f: (R, Tree) => R): R =
    f(z, this)
}

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
