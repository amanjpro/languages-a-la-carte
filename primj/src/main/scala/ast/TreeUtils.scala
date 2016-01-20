package ch.usi.inf.l3.sana.primj.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj

import tiny.ast._
import primj.ast.Implicits._
import calcj.ast._
import primj.ast._

import calcj.ast.operators._
import calcj.types._
import primj.symbols.VariableSymbol
import primj.modifiers.Ops._

trait TreeUtils extends calcj.ast.TreeUtils {
  def isTypeUse(tree: UseTree): Boolean = tree match {
    case _: IdentApi                    => false
    case _: TypeUseApi                  => true
  }

  def isVariable(tree: Tree): Boolean = tree match {
    case _: ValDefApi                    => true
    case _                            =>
      tree.symbol match {
        case Some(_: VariableSymbol) => true
        case _                       => false
      }
  }


  // make sure that the guards are constant expressions Section 15.27
  // def isConstantExpression(e: Tree): Boolean = e match {
  //   case lit: LiteralApi                              => true
  //   case cst: CastApi                                 =>
  //     // permit casts to primitive and string
  //     // TODO: Change this in OOJ, to handle String too
  //     cst.tpt.tpe match {
  //       case Some(_: PrimitiveType) => isConstantExpression(cst.expr)
  //       case _                      => false
  //     }
  //   case u: UnaryApi    if u.op != Inc && u.op != Dec    =>
  //     isConstantExpression(u.expr)
  //   case b: BinaryApi                                    =>
  //     isConstantExpression(b.lhs) &&
  //       isConstantExpression(b.rhs)
  //   case id: IdentApi                                    =>
  //     isFinal(id) && isVariable(id)
  //   case _                                            => false
  //   // TODO: Add qualified Select later in ooj
  //   // TypeName.Identifier only, and only when Identifier is already
  //   // a final variable
  // }
  //

  def isFinal(tree: Tree): Boolean =
    tree.symbol.map(_.mods.isFinal).getOrElse(false)


  def isValidStatement(e: Tree): Boolean = {
    lazy val isStmt = e match {
      // Statements in primj: if, while, for, block, return, valdef
      // brokenj adds: Switch, continue, break
      case _: IfApi | _: WhileApi | _: ForApi | _: BlockApi |
         _: ReturnApi | _: ValDefApi | NoTree     => true
      case _                                => false
    }
    isValidStatementExpression(e) || isStmt
  }

  def isValidStatementExpression(e: Tree): Boolean = e match {
    case u: UnaryApi  if u.op == Inc || u.op == Dec => true
    case _: ApplyApi                       => true
    // case _: New                         => true
    case _: AssignApi                      => true
    case NoTree                         => true
    case _                              => false
  }

  def isValDefOrStatementExpression(v: Tree): Boolean = v match {
    case s: ValDefApi => true
    case e: Expr   => isValidStatementExpression(e)
    case _         => false
  }

  // INFO: Update this to Java as we go
  def isValidExpression(e: Tree): Boolean = e match {
    case _: LiteralApi | _: IdentApi | _: BinaryApi | _: UnaryApi |
         _: AssignApi | _: TernaryApi | _: ApplyApi              => true
    case _                                              => false
  }


  def allPathsReturn(expr: Tree): Boolean = expr match {
    case wile: WhileApi                     =>
      allPathsReturn(wile.body)
    case forloop: ForApi                    =>
      allPathsReturn(forloop.body)
    case ifelse: IfApi                      =>
      allPathsReturn(ifelse.thenp) &&
      allPathsReturn(ifelse.elsep)
    case block: BlockApi                    =>
      block.stmts match {
        case Nil         => false
        case stmts       => allPathsReturn(stmts.last)
      }
    case ret: ReturnApi                     =>
      true
    case _                               =>
      false
  }


  def isSimpleExpression(tree: Tree): Boolean = tree match {
    case _: WhileApi                 => false
    case _: ForApi                   => false
    case _: ValDefApi                => false
    case _: MethodDefApi             => false
    case _: ProgramApi               => false
    case _: ReturnApi                => false
    case _: IfApi                    => false
    case _: BlockApi                 => false
    case _: TypeUseApi               => false
    case _                        => true
  }


  def isConstantLiteral(tree: Tree): Boolean = tree match {
    case lit: Literal              =>
      true
    case _                         =>
      false
  }
}

object TreeUtils extends TreeUtils



