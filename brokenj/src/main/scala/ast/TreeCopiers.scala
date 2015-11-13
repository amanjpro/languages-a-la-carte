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

  def copyLabel(template: Label)(name: Name = template.name,
    stmt: Expr = template.stmt): Label = {
    val res = Label(name, stmt)
    copyProperties(template, res)
    res
  }

  def copyBreak(template: Break)(label: Option[Name] = template.label): Break = {
    val res = Break(label)
    copyProperties(template, res)
    res
  }

  def copyContinue(template: Continue)(label: Option[Name] =
        template.label): Continue = {
    val res = Continue(label)
    copyProperties(template, res)
    res
  }

  def copyCase(template: Case)(guards: List[Expr] = template.guards,
    body: Tree = template.body): Case = {
    val res = Case(guards, body)
    copyProperties(template, res)
    res
  }

  def copySwitch(template: Switch)(expr: Expr = template.expr,
    cases: List[CaseApi] = template.cases): Switch = {
    val res = Switch(expr, cases)
    copyProperties(template, res)
    res
  }

}

object TreeCopiers extends TreeCopiers


