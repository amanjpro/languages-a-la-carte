package ch.usi.inf.l3.sana.primj.namers

import ch.usi.inf.l3.sana
import sana.primj
import sana.tiny
import sana.calcj

import sana.core.TransformationComponent
import sana.dsl._
import tiny.ast._
import tiny.errors.ErrorReporting.{error,warning}
import tiny.symbols._
import calcj.ast._
import calcj.ast.operators.{Inc, Dec}
import primj.ast._
import primj.ast.TreeUtils
import primj.symbols._
import primj.modifiers.Ops._
import primj.errors.ErrorCodes._


// package symbolassigners {
//   import scala.language.implicitConversions
//   object Implicits {
//     // We need this to glue compilation phases together easier
//     implicit def tree2TreeOptionSymbolTuple(tree: Tree):
//       (Tree, Option[Symbol]) = (tree, None)
//   }
// }


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

trait SymbolAssignerComponent extends
  TransformationComponent[(Tree, Option[Symbol]), Tree] {
  def assign: ((Tree, Option[Symbol])) => Tree
}

@component(tree, owner)
trait ProgramSymbolAssignerComponent extends SymbolAssignerComponent {
  (program: Program)          => {
    val symbol = Some(ProgramSymbol)
    val newMembers =
      program.members.map(x => assign((x, symbol)).asInstanceOf[DefTree])
    program.copy(members = newMembers, symbol = symbol)
  }
}


@component(tree, owner)
trait MethodDefSymbolAssignerComponent extends SymbolAssignerComponent {
  (mthd: MethodDef)          => {
    val symbol  = MethodSymbol(noflags, mthd.name,
      Nil, None, owner)
    owner.foreach(sym => sym.declare(symbol))
    val opsym   = Some(symbol)
    val tpt     = assign((mthd.ret, opsym)).asInstanceOf[UseTree]
    val params  = mthd.params.map((x) =>
        assign((x, opsym)).asInstanceOf[ValDef])
    val body    = assign((mthd.body, opsym)).asInstanceOf[Expr]
    symbol.params = params.map(_.symbol).flatten

    mthd.copy(ret = tpt, params = params, body = body, symbol = opsym)
  }
}

@component(tree, owner)
trait ValDefSymbolAssignerComponent extends SymbolAssignerComponent {
  (valdef: ValDef)          => {
    val tpt     = assign((valdef.tpt, owner)).asInstanceOf[UseTree]
    val rhs     = assign((valdef.rhs, owner)).asInstanceOf[Expr]

    val symbol  = VariableSymbol(valdef.mods, valdef.name,
      tpt.tpe, owner)
    owner.foreach(sym => sym.declare(symbol))
    val opsym   = Some(symbol)
    valdef.copy(tpt = tpt, rhs = rhs, symbol = opsym)
  }
}


@component(tree, owner)
trait TypeUseSymbolAssignerComponent extends SymbolAssignerComponent {
  (tuse: TypeUse)          => {
    val symbol = owner.flatMap(_.getSymbol(tuse.name,
      _.isInstanceOf[TypeSymbol]))
    symbol match {
      case Some(sym)      => TypeUse(sym, tuse.pos)
      case _              => tuse
    }
  }
}

@component(tree, owner)
trait ForSymbolAssignerComponent extends SymbolAssignerComponent {
  (forloop: For)          => {
    val symbol  = Some(ScopeSymbol(owner))
    val inits = forloop.inits.map { init =>
      assign((init, symbol))
    }
    val cond = assign((forloop.cond, symbol)).asInstanceOf[Expr]
    val steps = forloop.steps.map { step =>
      assign((step, symbol)).asInstanceOf[Expr]
    }
    val body = assign((forloop.body, symbol)).asInstanceOf[Expr]
    forloop.copy(inits = inits, cond = cond, steps = steps,
      body = body, owner = owner, symbol = symbol)
  }
}

@component(tree, owner)
trait BlockSymbolAssignerComponent extends SymbolAssignerComponent {
  (block: Block)          => {
    val symbol  = Some(ScopeSymbol(owner))
    val stmts = block.stmts.map { stmt =>
      assign((stmt, symbol))
    }
    block.copy(stmts = stmts, owner = owner, symbol = symbol)
  }
}

// Boring cases, just pass the owner around and assign it to
// all the trees that can have an owner
@component(tree, owner)
trait IdentSymbolAssignerComponent extends SymbolAssignerComponent {
  (id: Ident)          => {
    Ident(id.name, id.pos, owner)
  }
}

@component(tree, owner)
trait BinarySymbolAssignerComponent extends SymbolAssignerComponent {
  (bin: Binary)          => {
    val lhs = assign((bin.lhs, owner)).asInstanceOf[Expr]
    val rhs = assign((bin.rhs, owner)).asInstanceOf[Expr]
    bin.copy(lhs = lhs, rhs = rhs, owner = owner)
  }
}

@component(tree, owner)
trait UnarySymbolAssignerComponent extends SymbolAssignerComponent {
  (unary: Unary)          => {
    val expr = assign((unary.expr, owner)).asInstanceOf[Expr]
    unary.copy(expr = expr, owner = owner)
  }
}

@component(tree, owner)
trait CastSymbolAssignerComponent extends SymbolAssignerComponent {
  (cast: Cast)          => {
    val expr = assign((cast.expr, owner)).asInstanceOf[Expr]
    cast.copy(expr = expr, owner = owner)
  }
}

@component(tree, owner)
trait ReturnSymbolAssignerComponent extends SymbolAssignerComponent {
  (ret: Return)          => {
    val expr = ret.expr.map( x =>
      assign((x, owner)).asInstanceOf[Expr])
    ret.copy(expr = expr, owner = owner)
  }
}

@component(tree, owner)
trait AssignSymbolAssignerComponent extends SymbolAssignerComponent {
  (assgn: Assign)          => {
    val lhs = assign((assgn.lhs, owner)).asInstanceOf[Expr]
    val rhs = assign((assgn.rhs, owner)).asInstanceOf[Expr]
    assgn.copy(lhs = lhs, rhs = rhs, owner = owner)
  }
}


@component(tree, owner)
trait TernarySymbolAssignerComponent extends SymbolAssignerComponent {
  (tern: Ternary)          => {
    val cond = assign((tern.cond, owner)).asInstanceOf[Expr]
    val thenp = assign((tern.thenp, owner)).asInstanceOf[Expr]
    val elsep = assign((tern.elsep, owner)).asInstanceOf[Expr]
    tern.copy(cond = cond, thenp = thenp, elsep = elsep, owner = owner)
  }
}

@component(tree, owner)
trait IfSymbolAssignerComponent extends SymbolAssignerComponent {
  (ifelse: If)          => {
    val cond = assign((ifelse.cond, owner)).asInstanceOf[Expr]
    val thenp = assign((ifelse.thenp, owner)).asInstanceOf[Expr]
    val elsep = assign((ifelse.elsep, owner)).asInstanceOf[Expr]
    ifelse.copy(cond = cond, thenp = thenp,
      elsep = elsep, owner = owner)
  }
}

@component(tree, owner)
trait WhileSymbolAssignerComponent extends SymbolAssignerComponent {
  (wile: While)          => {
    val cond = assign((wile.cond, owner)).asInstanceOf[Expr]
    val body = assign((wile.body, owner)).asInstanceOf[Expr]
    wile.copy(cond = cond, body = body, owner = owner)
  }
}

@component(tree, owner)
trait ApplySymbolAssignerComponent extends SymbolAssignerComponent {
  (apply: Apply)          => {
    val fun = assign((apply.fun, owner)).asInstanceOf[Expr]
    val args = apply.args.map { arg =>
      assign((arg, owner)).asInstanceOf[Expr]
    }
    apply.copy(fun = fun, args = args, owner = owner)
  }
}

@component(tree, owner)
trait LiteralSymbolAssignerComponent extends SymbolAssignerComponent {
  (lit: Literal) => lit
}

