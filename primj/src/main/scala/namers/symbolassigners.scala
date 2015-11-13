package ch.usi.inf.l3.sana.primj.namers

import ch.usi.inf.l3.sana
import sana.primj
import sana.tiny
import sana.calcj

import sana.core.TransformationComponent
import sana.dsl._
import tiny.ast.{TreeCopiers => _, _}
import tiny.ast.Implicits._
import tiny.errors.ErrorReporting.{error,warning}
import tiny.symbols._
import calcj.ast.{TreeCopiers => _, _}
import calcj.ast.operators.{Inc, Dec}
import primj.ast._
import primj.ast.TreeUtils
import primj.ast.TreeFactories._
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
  (program: ProgramApi)          => {
    val symbol = ProgramSymbol
    val newMembers =
      program.members.map(x => assign((x, Some(symbol))).asInstanceOf[DefTree])
    program.symbol = symbol
    symbol.owner.foreach(program.owner = _)
    TreeCopiers.copyProgram(program)(members = newMembers)
  }
}


@component(tree, owner)
trait MethodDefSymbolAssignerComponent extends SymbolAssignerComponent {
  (mthd: MethodDefApi)          => {
    val symbol  = MethodSymbol(noflags, mthd.name,
      Nil, None, owner)
    owner.foreach(sym => sym.declare(symbol))
    val opsym   = Some(symbol)
    val tpt     = assign((mthd.ret, owner)).asInstanceOf[UseTree]
    val params  = mthd.params.map((x) =>
        assign((x, opsym)).asInstanceOf[ValDefApi])
    val body    = assign((mthd.body, opsym)).asInstanceOf[Expr]
    symbol.params = params.map(_.symbol).flatten
    mthd.symbol = symbol
    symbol.owner.foreach(mthd.owner = _)

    TreeCopiers.copyMethodDef(mthd)(ret = tpt,
      params = params, body = body)
  }
}

@component(tree, owner)
trait ValDefSymbolAssignerComponent extends SymbolAssignerComponent {
  (valdef: ValDefApi)          => {
    val tpt     = assign((valdef.tpt, owner)).asInstanceOf[UseTree]
    val rhs     = assign((valdef.rhs, owner)).asInstanceOf[Expr]

    val symbol  = VariableSymbol(valdef.mods, valdef.name,
      tpt.tpe, owner)
    owner.foreach(sym => sym.declare(symbol))
    valdef.symbol = symbol
    symbol.owner.foreach(valdef.owner = _)
    TreeCopiers.copyValDef(valdef)(tpt = tpt, rhs = rhs)
  }
}


@component(tree, owner)
trait TypeUseSymbolAssignerComponent extends SymbolAssignerComponent {
  (tuse: TypeUseApi)          => {
    val symbol = owner.flatMap(_.getSymbol(tuse.name,
      _.isInstanceOf[TypeSymbol]))
    symbol match {
      case Some(sym)      =>
        owner.foreach(tuse.owner = _)
        tuse
      case _              => tuse
    }
  }
}

@component(tree, owner)
trait ForSymbolAssignerComponent extends SymbolAssignerComponent {
  (forloop: ForApi)          => {
    val symbol  = ScopeSymbol(owner)
    val opsym   = Some(symbol)
    val inits = forloop.inits.map { init =>
      assign((init, opsym))
    }
    val cond = assign((forloop.cond, opsym)).asInstanceOf[Expr]
    val steps = forloop.steps.map { step =>
      assign((step, opsym)).asInstanceOf[Expr]
    }
    val body = assign((forloop.body, opsym)).asInstanceOf[Expr]
    forloop.symbol = symbol
    symbol.owner.foreach(forloop.owner = _)
    TreeCopiers.copyFor(forloop)(inits = inits, cond = cond,
      steps = steps, body = body)
  }
}

@component(tree, owner)
trait BlockSymbolAssignerComponent extends SymbolAssignerComponent {
  (block: BlockApi)          => {
    val symbol  = ScopeSymbol(owner)
    val stmts = block.stmts.map { stmt =>
      assign((stmt, Some(symbol)))
    }
    block.symbol = symbol
    symbol.owner.foreach(block.owner = _)
    TreeCopiers.copyBlock(block)(stmts = stmts)
  }
}

// Boring cases, just pass the owner around and assign it to
// all the trees that can have an owner
@component(tree, owner)
trait IdentSymbolAssignerComponent extends SymbolAssignerComponent {
  (id: IdentApi)          => {
    owner.foreach(id.owner = _)
    id
  }
}

@component(tree, owner)
trait BinarySymbolAssignerComponent extends SymbolAssignerComponent {
  (bin: BinaryApi)          => {
    val lhs = assign((bin.lhs, owner)).asInstanceOf[Expr]
    val rhs = assign((bin.rhs, owner)).asInstanceOf[Expr]
    owner.foreach(bin.owner = _)
    TreeCopiers.copyBinary(bin)(lhs = lhs, rhs = rhs)
  }
}

@component(tree, owner)
trait UnarySymbolAssignerComponent extends SymbolAssignerComponent {
  (unary: UnaryApi)          => {
    val expr = assign((unary.expr, owner)).asInstanceOf[Expr]
    owner.foreach(unary.owner = _)
    TreeCopiers.copyUnary(unary)(expr = expr)
  }
}

@component(tree, owner)
trait CastSymbolAssignerComponent extends SymbolAssignerComponent {
  (cast: CastApi)          => {
    val expr = assign((cast.expr, owner)).asInstanceOf[Expr]
    owner.foreach(cast.owner = _)
    TreeCopiers.copyCast(cast)(expr = expr)
  }
}

@component(tree, owner)
trait ReturnSymbolAssignerComponent extends SymbolAssignerComponent {
  (ret: ReturnApi)          => {
    val expr = ret.expr.map( x =>
      assign((x, owner)).asInstanceOf[Expr])
    owner.foreach(ret.owner = _)
    TreeCopiers.copyReturn(ret)(expr = expr)
  }
}

@component(tree, owner)
trait AssignSymbolAssignerComponent extends SymbolAssignerComponent {
  (assgn: AssignApi)          => {
    val lhs = assign((assgn.lhs, owner)).asInstanceOf[Expr]
    val rhs = assign((assgn.rhs, owner)).asInstanceOf[Expr]
    owner.foreach(assgn.owner = _)
    TreeCopiers.copyAssign(assgn)(lhs = lhs, rhs = rhs)
  }
}


@component(tree, owner)
trait TernarySymbolAssignerComponent extends SymbolAssignerComponent {
  (tern: TernaryApi)          => {
    val cond = assign((tern.cond, owner)).asInstanceOf[Expr]
    val thenp = assign((tern.thenp, owner)).asInstanceOf[Expr]
    val elsep = assign((tern.elsep, owner)).asInstanceOf[Expr]
    owner.foreach(tern.owner = _)
    TreeCopiers.copyTernary(tern)(cond = cond,
      thenp = thenp, elsep = elsep)
  }
}

@component(tree, owner)
trait IfSymbolAssignerComponent extends SymbolAssignerComponent {
  (ifelse: IfApi)          => {
    val cond = assign((ifelse.cond, owner)).asInstanceOf[Expr]
    val thenp = assign((ifelse.thenp, owner)).asInstanceOf[Expr]
    val elsep = assign((ifelse.elsep, owner)).asInstanceOf[Expr]
    owner.foreach(ifelse.owner = _)
    TreeCopiers.copyIf(ifelse)(cond = cond, thenp = thenp,
      elsep = elsep)
  }
}

@component(tree, owner)
trait WhileSymbolAssignerComponent extends SymbolAssignerComponent {
  (wile: WhileApi)          => {
    val cond = assign((wile.cond, owner)).asInstanceOf[Expr]
    val body = assign((wile.body, owner)).asInstanceOf[Expr]
    owner.foreach(wile.owner = _)
    TreeCopiers.copyWhile(wile)(cond = cond, body = body)
  }
}

@component(tree, owner)
trait ApplySymbolAssignerComponent extends SymbolAssignerComponent {
  (apply: ApplyApi)          => {
    val fun = assign((apply.fun, owner)).asInstanceOf[Expr]
    val args = apply.args.map { arg =>
      assign((arg, owner)).asInstanceOf[Expr]
    }
    owner.foreach(apply.owner = _)
    TreeCopiers.copyApply(apply)(fun = fun, args = args)
  }
}

@component(tree, owner)
trait LiteralSymbolAssignerComponent extends SymbolAssignerComponent {
  (lit: LiteralApi) => lit
}

