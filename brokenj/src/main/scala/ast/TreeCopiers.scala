package ch.usi.inf.l3.sana.brokenj.ast

import ch.usi.inf.l3.sana
import sana.tiny.types.Type
import sana.tiny.source.Position
import sana.tiny.symbols.Symbol
import sana.tiny.names.Name
import sana.tiny.ast.Implicits._
import sana.tiny.modifiers.Flags
import sana.tiny.ast._
import sana.calcj.ast._
import sana.primj.ast._



trait TreeCopiers extends sana.primj.ast.TreeCopiers {

  def copyLabel(template: LabelApi)(name: Name = template.name,
    stmt: Expr = template.stmt): LabelApi = {
    val res = Label(name, stmt)
    copyProperties(template, res)
    res
  }

  def copyBreak(template: BreakApi)(label: Option[Name] =
    template.label): BreakApi = {
    val res = Break(label)
    copyProperties(template, res)
    res
  }

  def copyContinue(template: ContinueApi)(label: Option[Name] =
        template.label): ContinueApi = {
    val res = Continue(label)
    copyProperties(template, res)
    res
  }

  def copyCase(template: CaseApi)(guards: List[Expr] = template.guards,
    body: Tree = template.body): CaseApi = {
    val res = Case(guards, body)
    copyProperties(template, res)
    res
  }

  def copySwitch(template: SwitchApi)(expr: Expr = template.expr,
    cases: List[CaseApi] = template.cases): SwitchApi = {
    val res = Switch(expr, cases)
    copyProperties(template, res)
    res
  }

}

object TreeCopiers extends TreeCopiers

