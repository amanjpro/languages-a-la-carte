package ch.usi.inf.l3.sana.brokenj.namers

import ch.usi.inf.l3.sana
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj

import tiny.core.TransformationComponent
import tiny.dsl._
import tiny.ast.{TreeCopiers => _, _}
import primj.ast.Implicits._
import tiny.symbols._
import brokenj.ast._
import calcj.ast.operators.{Inc, Dec}
import tiny.errors.ErrorReporting.{error,warning}
import primj.ast.{TreeCopiers => _, _}
import primj.ast.TreeUtils
import primj.symbols._
import primj.namers.NamerComponent
import primj.modifiers.Ops._
import primj.errors.ErrorCodes._

/*
Done from primj
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

/*
Case: DONE
Swtich: DONE
Label: DONE
Break: DONE
Continue: DONE
*/

// @component
// trait CaseNamerComponent extends NamerComponent {
//   (cse: CaseApi)            => {
//     val guards =
//       cse.guards.map(x => name(x).asInstanceOf[Expr])
//     val body   = name(cse.body)
//     TreeCopiers.copyCase(cse)(guards = guards, body = body)
//   }
// }
//
// @component
// trait SwitchNamerComponent extends NamerComponent {
//   (switch: SwitchApi)            => {
//     val cases =
//       switch.cases.map(x => name(x).asInstanceOf[CaseApi])
//     val expr   = name(switch.expr).asInstanceOf[Expr]
//     TreeCopiers.copySwitch(switch)(cases = cases, expr = expr)
//   }
// }
//
// @component
// trait LabelNamerComponent extends NamerComponent {
//   (label: LabelApi)            => {
//     val stmt   = name(label.stmt).asInstanceOf[Expr]
//     TreeCopiers.copyLabel(label)(stmt = stmt)
//   }
// }
//
// @component
// trait BreakNamerComponent extends NamerComponent {
//   (break: BreakApi)            => break
// }
//
// @component
// trait ContinueNamerComponent extends NamerComponent {
//   (continue: ContinueApi) => continue
// }
//
