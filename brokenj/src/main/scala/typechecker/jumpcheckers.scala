package ch.usi.inf.l3.sana.brokenj.typechecker


import ch.usi.inf.l3.sana
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj

import sana.core.CheckerComponent
import sana.dsl._
import tiny.ast._
import tiny.ast.Implicits._
import tiny.names.Name
import calcj.ast._
import calcj.ast.operators.{Inc, Dec}
import tiny.errors.ErrorReporting.{error,warning}
import primj.ast._
import primj.symbols._
import primj.modifiers.Ops._
import primj.typechecker.ShapeCheckerComponent
import brokenj.ast._
import brokenj.errors.ErrorCodes._
import brokenj.ast.TreeUtils

/*
Program: DONE
MethodDef: DONE
ValDef: DONE
Assign: DONE
If: DONE
While: DONE
Block: DONE
For: DONE
Ternary: DONE
Apply: DONE
Return: DONE
Ident: DONE
NoTree: DONE
TypeUse: DONE
Literal: DONE
Cast: DONE
Binary: DONE
Unary: DONE
Switch: DONE
Case: DONE
Label: DONE
Break: DONE
Continue: DONE
*/



trait JumpCheckerComponent extends
  CheckerComponent[(Tree, List[Tree])] {
  def check: ((Tree, List[Tree])) => Unit
}


@component(tree, encls)
trait ContinueJumpCheckerComponent extends JumpCheckerComponent {
  (cont: Continue) =>
    if(encls.filter(isContinuable(_)) != Nil)
      error(BAD_CONTINUE_STMT,
        cont.toString, cont.toString, cont.pos, cont)
    else
      ()

  private def isContinuable(tree: Tree): Boolean =
    TreeUtils.isContinuable(tree)
}

@component(tree, encls)
trait BreakJumpCheckerComponent extends JumpCheckerComponent {
  (break: Break) =>
    if(encls.filter(isBreakable(_)) != Nil)
      error(BAD_BREAK_STMT,
        break.toString, break.toString, break.pos, break)
    else
      ()

  private def isBreakable(tree: Tree): Boolean =
    TreeUtils.isBreakable(tree)
}



@component(tree, encls)
trait LabelJumpCheckerComponent extends JumpCheckerComponent {
  (label: Label) => check((label.stmt, encls))
}


// boring cases
@component(tree, encls)
trait ProgramJumpCheckerComponent extends JumpCheckerComponent {
  (prog: Program) =>
    prog.members.map(member => check((member, Nil)))
}

@component(tree, encls)
trait MethodDefJumpCheckerComponent extends JumpCheckerComponent {
  (mthd: MethodDef) =>
    check((mthd.body, encls))
}

@component(tree, encls)
trait ValDefJumpCheckerComponent extends JumpCheckerComponent {
  (vdef: ValDef) =>
    check((vdef.rhs, encls))
}

@component(tree, encls)
trait AssignJumpCheckerComponent extends JumpCheckerComponent {
  (assgn: Assign) => {
    check((assgn.lhs, encls))
    check((assgn.rhs, encls))
  }
}

@component(tree, encls)
trait ReturnJumpCheckerComponent extends JumpCheckerComponent {
  (ret: Return) => ret.expr.foreach(expr => check((expr, encls)))
}

@component(tree, encls)
trait IfJumpCheckerComponent extends JumpCheckerComponent {
  (ifelse: If) => {
    check((ifelse.cond, encls))
    check((ifelse.thenp, encls))
    check((ifelse.elsep, encls))
  }
}

@component(tree, encls)
trait WhileJumpCheckerComponent extends JumpCheckerComponent {
  (wile: While) => {
    check((wile.cond, encls))
    check((wile.body, wile::encls))
  }
}

@component(tree, encls)
trait BlockJumpCheckerComponent extends JumpCheckerComponent {
  (block: Block) => {
    block.stmts.foreach(stmt => check((stmt, encls)))
  }
}

@component(tree, encls)
trait ForJumpCheckerComponent extends JumpCheckerComponent {
  (forloop: For) => {
    forloop.inits.foreach(init => check((init, encls)))
    check((forloop.cond, encls))
    forloop.steps.foreach(step => check((step, encls)))
    check((forloop.body, forloop::encls))
  }
}

@component(tree, encls)
trait TernaryJumpCheckerComponent extends JumpCheckerComponent {
  (ternary: Ternary) => {
    check((ternary.cond, encls))
    check((ternary.thenp, encls))
    check((ternary.elsep, encls))
  }
}

@component(tree, encls)
trait ApplyJumpCheckerComponent extends JumpCheckerComponent {
  (apply: Apply) => {
    check((apply.fun, encls))
    apply.args.foreach(arg => check((arg, encls)))
  }
}

@component(tree, encls)
trait CastJumpCheckerComponent extends JumpCheckerComponent {
  (cast: Cast) => {
    check((cast.expr, encls))
  }
}

@component(tree, encls)
trait UnaryJumpCheckerComponent extends JumpCheckerComponent {
  (unary: Unary) => {
    check((unary.expr, encls))
  }
}

@component(tree, encls)
trait BinaryJumpCheckerComponent extends JumpCheckerComponent {
  (binary: Binary) => {
    check((binary.lhs, encls))
    check((binary.rhs, encls))
  }
}


@component(tree, encls)
trait SwitchJumpCheckerComponent extends JumpCheckerComponent {
  (switch: Switch) => {
    check((switch.expr, encls))
    switch.cases.foreach(cse => check((cse, switch::encls)))
  }
}


@component(tree, encls)
trait CaseJumpCheckerComponent extends JumpCheckerComponent {
  (cse: Case) => {
    cse.guards.foreach(guard => check((guard, encls)))
    check((cse.body, encls))
  }
}

// even more boring cases
@component(tree, encls)
trait IdentJumpCheckerComponent extends JumpCheckerComponent {
  (ident: Ident) => ident
}

@component(tree, encls)
trait TypeUseJumpCheckerComponent extends JumpCheckerComponent {
  (tuse: TypeUse) => tuse
}



@component(tree, encls)
trait LiteralJumpCheckerComponent extends JumpCheckerComponent {
  (lit: Literal) => lit
}

// @component(tree, encls)
// trait NoTreeJumpCheckerComponent extends JumpCheckerComponent {
//   (ntree: NoTree) => ntree
// }
