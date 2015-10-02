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

trait ProgramSymbolAssignerComponent extends SymbolAssignerComponent {

  def apply(p: (Tree, Option[Symbol])): Tree = {
    val (tree, owner) = p
    tree match {
      case program: Program          =>
        val symbol = Some(ProgramSymbol)
        val newMembers =
          program.members.map(x => assign((x, symbol)).asInstanceOf[DefTree])
        program.copy(members = newMembers, symbol = symbol)
    }
  }

  def isDefinedAt(p: (Tree, Option[Symbol])): Boolean = p match {
    case (_: Program, _) => true
    case _               => false
  }
}


trait MethodDefSymbolAssignerComponent extends SymbolAssignerComponent {
  def apply(p: (Tree, Option[Symbol])): Tree = {
    val (tree, owner) = p
    tree match {
      case mthd: MethodDef          =>
        val symbol  = MethodSymbol(noflags, mthd.name,
          Nil, None, owner)
        val opsym   = Some(symbol)
        val tpt     = assign((mthd.ret, opsym)).asInstanceOf[UseTree]
        val params  = mthd.params.map((x) =>
            assign((x, opsym)).asInstanceOf[ValDef])
        val body    = assign((mthd.body, opsym)).asInstanceOf[Expr]
        symbol.params = params.map(_.symbol).flatten

        owner.foreach(sym => sym.declare(symbol))

        mthd.copy(ret = tpt, params = params, body = body, symbol = opsym)
    }
  }

  def isDefinedAt(p: (Tree, Option[Symbol])): Boolean = p match {
    case (_: MethodDef, _) => true
    case _                 => false
  }
}


trait ValDefSymbolAssignerComponent extends SymbolAssignerComponent {
  def apply(p: (Tree, Option[Symbol])): Tree = {
    val (tree, owner) = p
    tree match {
      case valdef: ValDef          =>
        val symbol  = VariableSymbol(valdef.mods, valdef.name,
          None, owner)
        val opsym   = Some(symbol)
        val tpt     = assign((valdef.tpt, opsym)).asInstanceOf[UseTree]
        val rhs     = assign((valdef.rhs, opsym)).asInstanceOf[Expr]

        owner.foreach(sym => sym.declare(symbol))
        valdef.copy(tpt = tpt, rhs = rhs, symbol = opsym)
    }
  }

  def isDefinedAt(p: (Tree, Option[Symbol])): Boolean = p match {
    case (_: ValDef, _) => true
    case _              => false
  }
}


trait TypeUseSymbolAssignerComponent extends SymbolAssignerComponent {
  def apply(p: (Tree, Option[Symbol])): Tree = {
    val (tree, owner) = p
    tree match {
      case tuse: TypeUse          =>
        val symbol = owner.flatMap(_.getSymbol(tuse.name,
          _.isInstanceOf[TypeSymbol]))
        symbol match {
          case Some(sym)      => TypeUse(sym, tuse.pos)
          case _              => tuse
        }
    }
  }

  def isDefinedAt(p: (Tree, Option[Symbol])): Boolean = p match {
    case (_: TypeUse, _) => true
    case _               => false
  }
}

trait ForSymbolAssignerComponent extends SymbolAssignerComponent {
  def apply(p: (Tree, Option[Symbol])): Tree = {
    val (tree, owner) = p
    tree match {
      case forloop: For          =>
        val symbol  = Some(ScopeSymbol(owner))
        val inits = forloop.inits.map { init =>
          assign((init, symbol)).asInstanceOf[Expr]
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

  def isDefinedAt(p: (Tree, Option[Symbol])): Boolean = p match {
    case (_: For, _)     => true
    case _               => false
  }
}

trait BlockSymbolAssignerComponent extends SymbolAssignerComponent {
  def apply(p: (Tree, Option[Symbol])): Tree = {
    val (tree, owner) = p
    tree match {
      case block: Block          =>
        val symbol  = Some(ScopeSymbol(owner))
        val stmts = block.stmts.map { stmt =>
          assign((stmt, symbol))
        }
        block.copy(stmts = stmts, owner = owner, symbol = symbol)
    }
  }

  def isDefinedAt(p: (Tree, Option[Symbol])): Boolean = p match {
    case (_: Block, _)   => true
    case _               => false
  }
}
// Boring cases, just pass the owner around and assign it to
// all the trees that can have an owner
trait IdentSymbolAssignerComponent extends SymbolAssignerComponent {
  def apply(p: (Tree, Option[Symbol])): Tree = {
    val (tree, owner) = p
    tree match {
      case id: Ident          =>
        Ident(id.symbol, id.pos, owner)
    }
  }

  def isDefinedAt(p: (Tree, Option[Symbol])): Boolean = p match {
    case (_: Ident, _)   => true
    case _               => false
  }
}

trait BinarySymbolAssignerComponent extends SymbolAssignerComponent {
  def apply(p: (Tree, Option[Symbol])): Tree = {
    val (tree, owner) = p
    tree match {
      case bin: Binary          =>
        val lhs = assign((bin.lhs, owner)).asInstanceOf[Expr]
        val rhs = assign((bin.rhs, owner)).asInstanceOf[Expr]
        bin.copy(lhs = lhs, rhs = rhs, owner = owner)
    }
  }

  def isDefinedAt(p: (Tree, Option[Symbol])): Boolean = p match {
    case (_: Binary, _)  => true
    case _               => false
  }
}

trait UnarySymbolAssignerComponent extends SymbolAssignerComponent {
  def apply(p: (Tree, Option[Symbol])): Tree = {
    val (tree, owner) = p
    tree match {
      case unary: Unary          =>
        val expr = assign((unary.expr, owner)).asInstanceOf[Expr]
        unary.copy(expr = expr, owner = owner)
    }
  }

  def isDefinedAt(p: (Tree, Option[Symbol])): Boolean = p match {
    case (_: Unary, _)   => true
    case _               => false
  }
}

trait CastSymbolAssignerComponent extends SymbolAssignerComponent {
  def apply(p: (Tree, Option[Symbol])): Tree = {
    val (tree, owner) = p
    tree match {
      case cast: Cast          =>
        val expr = assign((cast.expr, owner)).asInstanceOf[Expr]
        cast.copy(expr = expr, owner = owner)
    }
  }

  def isDefinedAt(p: (Tree, Option[Symbol])): Boolean = p match {
    case (_: Cast, _)    => true
    case _               => false
  }
}

trait ReturnSymbolAssignerComponent extends SymbolAssignerComponent {
  def apply(p: (Tree, Option[Symbol])): Tree = {
    val (tree, owner) = p
    tree match {
      case ret: Return          =>
        val expr = ret.expr.map( x =>
          assign((x, owner)).asInstanceOf[Expr])
        ret.copy(expr = expr, owner = owner)
    }
  }

  def isDefinedAt(p: (Tree, Option[Symbol])): Boolean = p match {
    case (_: Return, _)  => true
    case _               => false
  }
  def isDefinedAt(tree: Tree): Boolean = defines(tree, "Return")
}

trait AssignSymbolAssignerComponent extends SymbolAssignerComponent {
  def apply(p: (Tree, Option[Symbol])): Tree = {
    val (tree, owner) = p
    tree match {
      case assgn: Assign          =>
        val lhs = assign((assgn.lhs, owner)).asInstanceOf[Expr]
        val rhs = assign((assgn.rhs, owner)).asInstanceOf[Expr]
        assgn.copy(lhs = lhs, rhs = rhs, owner = owner)
    }
  }

  def isDefinedAt(p: (Tree, Option[Symbol])): Boolean = p match {
    case (_: Assign, _)  => true
    case _               => false
  }
  def isDefinedAt(tree: Tree): Boolean = defines(tree, "Assign")
}


trait TernarySymbolAssignerComponent extends SymbolAssignerComponent {
  def apply(p: (Tree, Option[Symbol])): Tree = {
    val (tree, owner) = p
    tree match {
      case tern: Ternary          =>
        val cond = assign((tern.cond, owner)).asInstanceOf[Expr]
        val thenp = assign((tern.thenp, owner)).asInstanceOf[Expr]
        val elsep = assign((tern.elsep, owner)).asInstanceOf[Expr]
        tern.copy(cond = cond, thenp = thenp, elsep = elsep, owner = owner)
    }
  }

  def isDefinedAt(p: (Tree, Option[Symbol])): Boolean = p match {
    case (_: Ternary, _) => true
    case _               => false
  }
}

trait IfSymbolAssignerComponent extends SymbolAssignerComponent {
  def apply(p: (Tree, Option[Symbol])): Tree = {
    val (tree, owner) = p
    tree match {
      case ifelse: If          =>
        val cond = assign((ifelse.cond, owner)).asInstanceOf[Expr]
        val thenp = assign((ifelse.thenp, owner)).asInstanceOf[Expr]
        val elsep = assign((ifelse.elsep, owner)).asInstanceOf[Expr]
        ifelse.copy(cond = cond, thenp = thenp,
          elsep = elsep, owner = owner)
    }
  }

  def isDefinedAt(p: (Tree, Option[Symbol])): Boolean = p match {
    case (_: If, _)    => true
    case _             => false
  }
}

trait WhileSymbolAssignerComponent extends SymbolAssignerComponent {
  def apply(p: (Tree, Option[Symbol])): Tree = {
    val (tree, owner) = p
    tree match {
      case wile: While          =>
        val cond = assign((wile.cond, owner)).asInstanceOf[Expr]
        val body = assign((wile.body, owner)).asInstanceOf[Expr]
        wile.copy(cond = cond, body = body, owner = owner)
    }
  }

  def isDefinedAt(p: (Tree, Option[Symbol])): Boolean = p match {
    case (_: While, _) => true
    case _             => false
  }
}

trait ApplySymbolAssignerComponent extends SymbolAssignerComponent {
  def apply(p: (Tree, Option[Symbol])): Tree = {
    val (tree, owner) = p
    tree match {
      case apply: Apply          =>
        val fun = assign((apply.fun, owner)).asInstanceOf[Expr]
        val args = apply.args.map { arg =>
          assign((arg, owner)).asInstanceOf[Expr]
        }
        apply.copy(fun = fun, args = args, owner = owner)
    }
  }

  def isDefinedAt(p: (Tree, Option[Symbol])): Boolean = p match {
    case (_: Apply, _) => true
    case _             => false
  }
}
