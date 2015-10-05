package ch.usi.inf.l3.sana.primj.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj

import tiny.ast._
import calcj.ast._
import primj.ast._

import calcj.ast.operators._
import calcj.types._
import primj.symbols.VariableSymbol
import primj.modifiers.Ops._

trait TreeUtils {
  def isTypeUse(tree: UseTree): Boolean = tree match {
    case _: Ident                    => false
    case _: TypeUse                  => true
  }

  def isVariable(tree: Tree): Boolean = tree match {
    case _: ValDef                    => true
    case st: SymTree                  =>
      st.symbol match {
        case Some(_: VariableSymbol) => true
        case _                       => false
      }
    case _                            => false
  }


  // make sure that the guards are constant expressions Section 15.27
  def isConstantExpression(e: Tree): Boolean = e match {
    case lit: Literal                                 => true
    case Cast(tpt, e, _, _)                           =>
      // permit casts to primitive and string
      // TODO: Change this in OOJ, to handle String too
      tpt.tpe match {
        case Some(_: PrimitiveType) => isConstantExpression(e)
        case _                      => false
      }
    case u: Unary    if u.op != Inc && u.op != Dec    =>
      isConstantExpression(u.expr)
    case b: Binary                                    =>
      isConstantExpression(b.lhs) &&
        isConstantExpression(b.rhs)
    case id: Ident                                    =>
      isFinal(id) && isVariable(id)
    case _                                            => false
    // TODO: Add qualified Select later in ooj
    // TypeName.Identifier only, and only when Identifier is already
    // a final variable
  }


  def isFinal(tree: Tree): Boolean = tree match {
    case st: SymTree                  =>
      st.symbol.map(_.mods.isFinal).getOrElse(false)
    case _                            => false
  }


  def isValidStatement(e: Tree): Boolean = {
    lazy val isStmt = e match {
      // Statements in primj: if, while, for, block, return, valdef
      // brokenj adds: Switch, continue, break
      case _: If | _: While | _: For | _: Block |
         _: Return | _: ValDef | NoTree     => true
      case _                                => false
    }
    isValidStatementExpression(e) || isStmt
  }

  def isValidStatementExpression(e: Tree): Boolean = e match {
    case Unary(_, Inc, _, _, _, _)      => true
    case Unary(_, Dec, _, _, _, _)      => true
    case _: Apply                       => true
    // case _: New                         => true
    case _: Assign                      => true
    case NoTree                         => true
    case _                              => false
  }

  def isValDefOrStatementExpression(v: Tree): Boolean = v match {
    case s: ValDef => true
    case e: Expr   => isValidStatementExpression(e)
    case _         => false
  }

  // INFO: Update this to Java as we go
  def isValidExpression(e: Tree): Boolean = e match {
    case _: Literal | _: Ident | _: Binary | _: Unary |
         _: Assign | _: Ternary | _: Apply              => true
    case _                                              => false
  }


  def allPathsReturn(expr: Tree): Boolean = expr match {
    case wile: While                     =>
      allPathsReturn(wile.body)
    case forloop: For                    =>
      allPathsReturn(forloop.body)
    case ifelse: If                      =>
      allPathsReturn(ifelse.thenp) &&
      allPathsReturn(ifelse.elsep)
    case block: Block                    =>
      block.stmts match {
        case Nil         => false
        case stmts       => allPathsReturn(stmts.last)
      }
    case ret: Return                     =>
      true
    case _                               =>
      false
  }


  def isSimpleExpression(tree: Tree): Boolean = tree match {
    case _: While                 => false
    case _: For                   => false
    case _: ValDef                => false
    case _: MethodDef             => false
    case _: Program               => false
    case _: Return                => false
    case _: If                    => false
    case _: Block                 => false
    case _: TypeUse               => false
    case _                        => true
  }
}

object TreeUtils extends TreeUtils



