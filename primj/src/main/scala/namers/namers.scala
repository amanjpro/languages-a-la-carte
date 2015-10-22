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
  //def name[A <: Tree]: A => A = (a: A) => a
}

@component
trait ProgramNamerComponent extends NamerComponent {
  (program: Program)          => {
    val newMembers =
      program.members.map(x => name(x).asInstanceOf[DefTree])
    program.copy(members = newMembers)
  }

}

@component
trait MethodDefNamerComponent extends NamerComponent {
  (mthd: MethodDef)          => {
    val rhs = name(mthd.body).asInstanceOf[Expr]
    mthd.copy(body = rhs)
  }

}

@component
trait ValDefNamerComponent extends NamerComponent {
  (valdef: ValDef)          => {
    // Only local variables need to be named
    val tpt     = name(valdef.tpt).asInstanceOf[UseTree]
    val rhs     = name(valdef.rhs).asInstanceOf[Expr]

    valdef.copy(tpt = tpt, rhs = rhs)
  }

}


@component
trait TypeUseNamerComponent extends NamerComponent {
  (tuse: TypeUse)          => {
    val symbol = tuse.owner.flatMap(_.getSymbol(tuse.name,
      _.isInstanceOf[TypeSymbol]))
    symbol match {
      case Some(sym)      => TypeUse(sym, tuse.pos)
      case _              => tuse
    }
  }

}

@component
trait IdentNamerComponent extends NamerComponent {
  (id: Ident)          => {
    val symbol = id.owner.flatMap(_.getSymbol(id.name,
      _.isInstanceOf[TermSymbol]))
    symbol match {
      case Some(sym)      => Ident(sym, id.pos)
      case _              => id
    }
  }

}

@component
trait ForNamerComponent extends NamerComponent {
  (forloop: For)          => {
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

}

@component
trait BlockNamerComponent extends NamerComponent {
  (block: Block)          => {
    val stmts = block.stmts.map { stmt => name(stmt) }
    block.copy(stmts = stmts)
  }

}
// Boring cases, just pass the owner around and name it to
// all the trees that can have an owner


@component
trait BinaryNamerComponent extends NamerComponent {
  (bin: Binary)          => {
    val lhs = name(bin.lhs).asInstanceOf[Expr]
    val rhs = name(bin.rhs).asInstanceOf[Expr]
    bin.copy(lhs = lhs, rhs = rhs)
  }

}

@component
trait UnaryNamerComponent extends NamerComponent {
  (unary: Unary)          => {
    val expr = name(unary.expr).asInstanceOf[Expr]
    unary.copy(expr = expr)
  }

}

@component
trait CastNamerComponent extends NamerComponent {
  (cast: Cast)          => {
    val expr = name(cast.expr).asInstanceOf[Expr]
    cast.copy(expr = expr)
  }

}

@component
trait ReturnNamerComponent extends NamerComponent {
  (ret: Return)          => {
    val expr = ret.expr.map(name(_).asInstanceOf[Expr])
    ret.copy(expr = expr)
  }

}

@component
trait AssignNamerComponent extends NamerComponent {
  (assgn: Assign)          => {
    val lhs = name(assgn.lhs).asInstanceOf[Expr]
    val rhs = name(assgn.rhs).asInstanceOf[Expr]
    assgn.copy(lhs = lhs, rhs = rhs)
  }

}


@component
trait TernaryNamerComponent extends NamerComponent {
  (tern: Ternary)          => {
    val cond = name(tern.cond).asInstanceOf[Expr]
    val thenp = name(tern.thenp).asInstanceOf[Expr]
    val elsep = name(tern.elsep).asInstanceOf[Expr]
    tern.copy(cond = cond, thenp = thenp, elsep = elsep)
  }

}

@component
trait IfNamerComponent extends NamerComponent {
  (ifelse: If)          => {
    val cond = name(ifelse.cond).asInstanceOf[Expr]
    val thenp = name(ifelse.thenp).asInstanceOf[Expr]
    val elsep = name(ifelse.elsep).asInstanceOf[Expr]
    ifelse.copy(cond = cond, thenp = thenp,
      elsep = elsep)
  }

}

@component
trait WhileNamerComponent extends NamerComponent {
  (wile: While)          => {
    val cond = name(wile.cond).asInstanceOf[Expr]
    val body = name(wile.body).asInstanceOf[Expr]
    wile.copy(cond = cond, body = body)
  }

}

@component
trait ApplyNamerComponent extends NamerComponent {
  (apply: Apply)          => {
    val fun = name(apply.fun).asInstanceOf[Expr]
    val args = apply.args.map { arg =>
      name(arg).asInstanceOf[Expr]
    }
    apply.copy(fun = fun, args = args)
  }

}

@component
trait LiteralNamerComponent extends NamerComponent {
  (lit: Literal)          => lit
}


