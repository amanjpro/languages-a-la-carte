package ch.usi.inf.l3.sana.brokenj.typechecker


import ch.usi.inf.l3.sana
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj

import sana.core.CheckerComponent
import sana.dsl._
import tiny.ast._
import primj.ast.Implicits._
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
  (cont: ContinueApi) =>
    if(encls.filter(isContinuable(_)) == Nil)
      error(BAD_CONTINUE_STMT,
        "", "", cont.pos)
    else
      ()

  private def isContinuable(tree: Tree): Boolean =
    TreeUtils.isContinuable(tree)
}

@component(tree, encls)
trait BreakJumpCheckerComponent extends JumpCheckerComponent {
  (break: BreakApi) =>
    if(encls.filter(isBreakable(_)) == Nil &&
        break.label == None)
      error(BAD_BREAK_STMT,
        "", "", break.pos)
    else
      ()

  private def isBreakable(tree: Tree): Boolean =
    TreeUtils.isBreakable(tree)
}



@component(tree, encls)
trait LabelJumpCheckerComponent extends JumpCheckerComponent {
  (label: LabelApi) => check((label.stmt, encls))
}


// boring cases
@component(tree, encls)
trait ProgramJumpCheckerComponent extends JumpCheckerComponent {
  (prog: ProgramApi) =>
    prog.members.map(member => check((member, Nil)))
}

@component(tree, encls)
trait MethodDefJumpCheckerComponent extends JumpCheckerComponent {
  (mthd: MethodDefApi) =>
    check((mthd.body, encls))
}

@component(tree, encls)
trait ValDefJumpCheckerComponent extends JumpCheckerComponent {
  (vdef: ValDefApi) =>
    check((vdef.rhs, encls))
}

@component(tree, encls)
trait AssignJumpCheckerComponent extends JumpCheckerComponent {
  (assgn: AssignApi) => {
    check((assgn.lhs, encls))
    check((assgn.rhs, encls))
  }
}

@component(tree, encls)
trait ReturnJumpCheckerComponent extends JumpCheckerComponent {
  (ret: ReturnApi) => ret.expr.foreach(expr => check((expr, encls)))
}

@component(tree, encls)
trait IfJumpCheckerComponent extends JumpCheckerComponent {
  (ifelse: IfApi) => {
    check((ifelse.cond, encls))
    check((ifelse.thenp, encls))
    check((ifelse.elsep, encls))
  }
}

@component(tree, encls)
trait WhileJumpCheckerComponent extends JumpCheckerComponent {
  (wile: WhileApi) => {
    check((wile.cond, encls))
    check((wile.body, wile::encls))
  }
}

@component(tree, encls)
trait BlockJumpCheckerComponent extends JumpCheckerComponent {
  (block: BlockApi) => {
    block.stmts.foreach(stmt => check((stmt, encls)))
  }
}

@component(tree, encls)
trait ForJumpCheckerComponent extends JumpCheckerComponent {
  (forloop: ForApi) => {
    forloop.inits.foreach(init => check((init, encls)))
    check((forloop.cond, encls))
    forloop.steps.foreach(step => check((step, encls)))
    check((forloop.body, forloop::encls))
  }
}

@component(tree, encls)
trait TernaryJumpCheckerComponent extends JumpCheckerComponent {
  (ternary: TernaryApi) => {
    check((ternary.cond, encls))
    check((ternary.thenp, encls))
    check((ternary.elsep, encls))
  }
}

@component(tree, encls)
trait ApplyJumpCheckerComponent extends JumpCheckerComponent {
  (apply: ApplyApi) => {
    check((apply.fun, encls))
    apply.args.foreach(arg => check((arg, encls)))
  }
}

@component(tree, encls)
trait CastJumpCheckerComponent extends JumpCheckerComponent {
  (cast: CastApi) => {
    check((cast.expr, encls))
  }
}

@component(tree, encls)
trait UnaryJumpCheckerComponent extends JumpCheckerComponent {
  (unary: UnaryApi) => {
    check((unary.expr, encls))
  }
}

@component(tree, encls)
trait BinaryJumpCheckerComponent extends JumpCheckerComponent {
  (binary: BinaryApi) => {
    check((binary.lhs, encls))
    check((binary.rhs, encls))
  }
}


@component(tree, encls)
trait SwitchJumpCheckerComponent extends JumpCheckerComponent {
  (switch: SwitchApi) => {
    check((switch.expr, encls))
    switch.cases.foreach(cse => check((cse, encls)))
  }
}


@component(tree, encls)
trait CaseJumpCheckerComponent extends JumpCheckerComponent {
  (cse: CaseApi) => {
    cse.guards.foreach(guard => check((guard, encls)))
    check((cse.body, cse::encls))
  }
}

// even more boring cases
@component(tree, encls)
trait IdentJumpCheckerComponent extends JumpCheckerComponent {
  (ident: IdentApi) => ident
}

@component(tree, encls)
trait TypeUseJumpCheckerComponent extends JumpCheckerComponent {
  (tuse: TypeUseApi) => tuse
}



@component(tree, encls)
trait LiteralJumpCheckerComponent extends JumpCheckerComponent {
  (lit: LiteralApi) => lit
}

// @component(tree, encls)
// trait NoTreeJumpCheckerComponent extends JumpCheckerComponent {
//   (ntree: NoTree) => ntree
// }
