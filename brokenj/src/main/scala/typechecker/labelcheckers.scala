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



trait LabelNameCheckerComponent extends
  CheckerComponent[(Tree, List[LabelApi])] {
  def check: ((Tree, List[LabelApi])) => Unit
}


@component(tree, labelNames)
trait ContinueLabelNameCheckerComponent extends LabelNameCheckerComponent {
  (cont: ContinueApi) => cont.label match {
    case None       => ()
    case Some(name) =>
      labelNames.filter(_.name == name) match {
        case Nil                                =>
          error(NO_LABEL_DEF,
            "", "", cont.pos)
        case (x::xs)   if isContinuable(x.stmt) =>
          ()
        case _                                  =>
          ()
          error(BAD_CONTINUE_STMT,
            "", "", cont.pos)
      }
  }

  private def isContinuable(tree: Tree): Boolean =
    TreeUtils.isContinuable(tree)
}

@component(tree, labelNames)
trait BreakLabelNameCheckerComponent extends LabelNameCheckerComponent {
  (break: BreakApi) => break.label match {
    case None       => ()
    case Some(name) =>
      labelNames.filter(_.name == name) match {
        case Nil                             =>
          error(NO_LABEL_DEF,
            "", "", break.pos)
        case _                               =>
          ()
      }
  }
}



@component(tree, labelNames)
trait LabelLabelNameCheckerComponent extends LabelNameCheckerComponent {
  (label: LabelApi) => {
    val name = label.name
    if(labelNames.exists(_.name == name)) {
      error(DOUBLE_LABEL_DEF,
        "", "", label.pos)
    } else ()
    check((label.stmt, label::labelNames))
  }
}


// boring cases
@component(tree, labelNames)
trait ProgramLabelNameCheckerComponent extends LabelNameCheckerComponent {
  (prog: ProgramApi) =>
    prog.members.map(member => check((member, Nil)))
}

@component(tree, labelNames)
trait MethodDefLabelNameCheckerComponent extends LabelNameCheckerComponent {
  (mthd: MethodDefApi) =>
    check((mthd.body, labelNames))
}

@component(tree, labelNames)
trait ValDefLabelNameCheckerComponent extends LabelNameCheckerComponent {
  (vdef: ValDefApi) =>
    check((vdef.rhs, labelNames))
}

@component(tree, labelNames)
trait AssignLabelNameCheckerComponent extends LabelNameCheckerComponent {
  (assgn: AssignApi) => {
    check((assgn.lhs, labelNames))
    check((assgn.rhs, labelNames))
  }
}

@component(tree, labelNames)
trait ReturnLabelNameCheckerComponent extends LabelNameCheckerComponent {
  (ret: ReturnApi) => ret.expr.foreach(expr => check((expr, labelNames)))
}

@component(tree, labelNames)
trait IfLabelNameCheckerComponent extends LabelNameCheckerComponent {
  (ifelse: IfApi) => {
    check((ifelse.cond, labelNames))
    check((ifelse.thenp, labelNames))
    check((ifelse.elsep, labelNames))
  }
}

@component(tree, labelNames)
trait WhileLabelNameCheckerComponent extends LabelNameCheckerComponent {
  (wile: WhileApi) => {
    check((wile.cond, labelNames))
    check((wile.body, labelNames))
  }
}

@component(tree, labelNames)
trait BlockLabelNameCheckerComponent extends LabelNameCheckerComponent {
  (block: BlockApi) => {
    block.stmts.foreach(stmt => check((stmt, labelNames)))
  }
}

@component(tree, labelNames)
trait ForLabelNameCheckerComponent extends LabelNameCheckerComponent {
  (forloop: ForApi) => {
    forloop.inits.foreach(init => check((init, labelNames)))
    check((forloop.cond, labelNames))
    forloop.steps.foreach(step => check((step, labelNames)))
    check((forloop.body, labelNames))
  }
}

@component(tree, labelNames)
trait TernaryLabelNameCheckerComponent extends LabelNameCheckerComponent {
  (ternary: TernaryApi) => {
    check((ternary.cond, labelNames))
    check((ternary.thenp, labelNames))
    check((ternary.elsep, labelNames))
  }
}

@component(tree, labelNames)
trait ApplyLabelNameCheckerComponent extends LabelNameCheckerComponent {
  (apply: ApplyApi) => {
    check((apply.fun, labelNames))
    apply.args.foreach(arg => check((arg, labelNames)))
  }
}

@component(tree, labelNames)
trait CastLabelNameCheckerComponent extends LabelNameCheckerComponent {
  (cast: CastApi) => {
    check((cast.expr, labelNames))
  }
}

@component(tree, labelNames)
trait UnaryLabelNameCheckerComponent extends LabelNameCheckerComponent {
  (unary: UnaryApi) => {
    check((unary.expr, labelNames))
  }
}

@component(tree, labelNames)
trait BinaryLabelNameCheckerComponent extends LabelNameCheckerComponent {
  (binary: BinaryApi) => {
    check((binary.lhs, labelNames))
    check((binary.rhs, labelNames))
  }
}


@component(tree, labelNames)
trait SwitchLabelNameCheckerComponent extends LabelNameCheckerComponent {
  (switch: SwitchApi) => {
    check((switch.expr, labelNames))
    switch.cases.foreach(cse => check((cse, labelNames)))
  }
}


@component(tree, labelNames)
trait CaseLabelNameCheckerComponent extends LabelNameCheckerComponent {
  (cse: CaseApi) => {
    cse.guards.foreach(guard => check((guard, labelNames)))
    check((cse.body, labelNames))
  }
}

// even more boring cases
@component(tree, labelNames)
trait IdentLabelNameCheckerComponent extends LabelNameCheckerComponent {
  (ident: IdentApi) => ident
}

@component(tree, labelNames)
trait TypeUseLabelNameCheckerComponent extends LabelNameCheckerComponent {
  (tuse: TypeUseApi) => tuse
}



@component(tree, labelNames)
trait LiteralLabelNameCheckerComponent extends LabelNameCheckerComponent {
  (lit: LiteralApi) => lit
}

// @component(tree, labelNames)
// trait NoTreeLabelNameCheckerComponent extends LabelNameCheckerComponent {
//   (ntree: NoTree) => ntree
// }
