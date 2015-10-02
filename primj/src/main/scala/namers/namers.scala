package ch.usi.inf.l3.sana.primj.namers

import ch.usi.inf.l3.sana
import sana.primj
import sana.tiny
import sana.calcj

import sana.core.TransformationComponent
import sana.dsl._
import tiny.ast._
import tiny.symbols._
import calcj.ast._
import calcj.ast.operators.{Inc, Dec}
import tiny.errors.ErrorReporting.{error,warning}
import primj.ast._
import primj.ast.TreeUtils
import primj.symbols._
import primj.modifiers.Ops._
import primj.errors.ErrorCodes._

/*
Program: DONE
Assign: DONE
If: DONE
While: DONE
Block: DONE
For: DONE
Ternary: DONE
Apply: DONE
Return: DONE
MethodDef: DONE
ValDef: DONE
Ident: DONE
NoTree: DONE
TypeUse: DONE
Cast: DONE
Binary: DONE
Literal: DONE
Unary: DONE
*/


trait NamerComponent extends TransformationComponent[Tree, Tree] {
  def name: Tree => Tree
}

trait ProgramNamerComponent extends NamerComponent {

  def apply(tree: Tree): Tree = tree match {
    case program: Program          =>
      val newMembers =
        program.members.map(x => name(x).asInstanceOf[DefTree])
      program.copy(members = newMembers)
  }

  def isDefinedAt(tree: Tree): Boolean   = defines(tree, "Program")
}

trait MethodDefNamerComponent extends NamerComponent {
  def apply(tree: Tree): Tree = tree match {
    case mthd: MethodDef          =>
      val rhs = name(mthd.body).asInstanceOf[Expr]
      mthd.copy(body = rhs)
  }

  def isDefinedAt(tree: Tree): Boolean = defines(tree, "MethodDef")
}

trait ValDefNamerComponent extends NamerComponent {
  def apply(tree: Tree): Tree = tree match {
    case valdef: ValDef          =>
      // Only local variables need to be named
      val tpt     = name(valdef.tpt).asInstanceOf[UseTree]
      val rhs     = name(valdef.rhs).asInstanceOf[Expr]

      valdef.copy(tpt = tpt, rhs = rhs)
  }

  def isDefinedAt(tree: Tree): Boolean = defines(tree, "ValDef")
}


trait TypeUseNamerComponent extends NamerComponent {
  def apply(tree: Tree): Tree = tree match {
    case tuse: TypeUse          =>
      val symbol = tuse.owner.flatMap(_.getSymbol(tuse.name,
        _.isInstanceOf[TypeSymbol]))
      symbol match {
        case Some(sym)      => TypeUse(sym, tuse.pos)
        case _              => tuse
      }
  }

  def isDefinedAt(tree: Tree): Boolean = defines(tree, "TypeUse")
}

trait IdentNamerComponent extends NamerComponent {
  def apply(tree: Tree): Tree = tree match {
    case id: Ident          =>
      val symbol = id.owner.flatMap(_.getSymbol(id.name,
        _.isInstanceOf[TermSymbol]))
      symbol match {
        case Some(sym)      => Ident(sym, id.pos)
        case _              => id
      }
  }

  def isDefinedAt(tree: Tree): Boolean = defines(tree, "Ident")
}

trait ForNamerComponent extends NamerComponent {
  def apply(tree: Tree): Tree = tree match {
    case forloop: For          =>
      val inits = forloop.inits.map { init =>
        name(init)
      }
      val cond = name(forloop.cond).asInstanceOf[Expr]
      val steps = forloop.steps.map { step =>
        name(step).asInstanceOf[Expr]
      }
      val body = name(forloop.body).asInstanceOf[Expr]
      forloop.copy(inits = inits, cond = cond, steps = steps,
        body = body)
  }

  def isDefinedAt(tree: Tree): Boolean = defines(tree, "For")
}

trait BlockNamerComponent extends NamerComponent {
  def apply(tree: Tree): Tree = tree match {
    case block: Block          =>
      val stmts = block.stmts.map { stmt => name(stmt) }
      block.copy(stmts = stmts)
  }

  def isDefinedAt(tree: Tree): Boolean = defines(tree, "Block")
}
// Boring cases, just pass the owner around and name it to
// all the trees that can have an owner


trait BinaryNamerComponent extends NamerComponent {
  def apply(tree: Tree): Tree = tree match {
    case bin: Binary          =>
      val lhs = name(bin.lhs).asInstanceOf[Expr]
      val rhs = name(bin.rhs).asInstanceOf[Expr]
      bin.copy(lhs = lhs, rhs = rhs)
  }

  def isDefinedAt(tree: Tree): Boolean = defines(tree, "Binary")
}

trait UnaryNamerComponent extends NamerComponent {
  def apply(tree: Tree): Tree = tree match {
    case unary: Unary          =>
      val expr = name(unary.expr).asInstanceOf[Expr]
      unary.copy(expr = expr)
  }

  def isDefinedAt(tree: Tree): Boolean = defines(tree, "Unary")
}

trait CastNamerComponent extends NamerComponent {
  def apply(tree: Tree): Tree = tree match {
    case cast: Cast          =>
      val expr = name(cast.expr).asInstanceOf[Expr]
      cast.copy(expr = expr)
  }

  def isDefinedAt(tree: Tree): Boolean = defines(tree, "Cast")
}

trait ReturnNamerComponent extends NamerComponent {
  def apply(tree: Tree): Tree = tree match {
    case ret: Return          =>
      val expr = ret.expr.map(name(_).asInstanceOf[Expr])
      ret.copy(expr = expr)
  }

  def isDefinedAt(tree: Tree): Boolean = defines(tree, "Return")
}

trait AssignNamerComponent extends NamerComponent {
  def apply(tree: Tree): Tree = tree match {
    case assgn: Assign          =>
      val lhs = name(assgn.lhs).asInstanceOf[Expr]
      val rhs = name(assgn.rhs).asInstanceOf[Expr]
      assgn.copy(lhs = lhs, rhs = rhs)
  }

  def isDefinedAt(tree: Tree): Boolean = defines(tree, "Assign")
}


trait TernaryNamerComponent extends NamerComponent {
  def apply(tree: Tree): Tree = tree match {
    case tern: Ternary          =>
      val cond = name(tern.cond).asInstanceOf[Expr]
      val thenp = name(tern.thenp).asInstanceOf[Expr]
      val elsep = name(tern.elsep).asInstanceOf[Expr]
      tern.copy(cond = cond, thenp = thenp, elsep = elsep)
  }

  def isDefinedAt(tree: Tree): Boolean = defines(tree, "Ternary")
}

trait IfNamerComponent extends NamerComponent {
  def apply(tree: Tree): Tree = tree match {
    case ifelse: If          =>
      val cond = name(ifelse.cond).asInstanceOf[Expr]
      val thenp = name(ifelse.thenp).asInstanceOf[Expr]
      val elsep = name(ifelse.elsep).asInstanceOf[Expr]
      ifelse.copy(cond = cond, thenp = thenp,
        elsep = elsep)
  }

  def isDefinedAt(tree: Tree): Boolean = defines(tree, "If")
}

trait WhileNamerComponent extends NamerComponent {
  def apply(tree: Tree): Tree = tree match {
    case wile: While          =>
      val cond = name(wile.cond).asInstanceOf[Expr]
      val body = name(wile.body).asInstanceOf[Expr]
      wile.copy(cond = cond, body = body)
  }

  def isDefinedAt(tree: Tree): Boolean = defines(tree, "While")
}

trait ApplyNamerComponent extends NamerComponent {
  def apply(tree: Tree): Tree = tree match {
    case apply: Apply          =>
      val fun = name(apply.fun).asInstanceOf[Expr]
      val args = apply.args.map { arg =>
        name(arg).asInstanceOf[Expr]
      }
      apply.copy(fun = fun, args = args)
  }

  def isDefinedAt(tree: Tree): Boolean = defines(tree, "Apply")
}
