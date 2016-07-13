/*
 * Copyright (c) <2015-2016>, see CONTRIBUTORS
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of the <organization> nor the
 *     names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package ch.usi.inf.l3.sana.primj.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj

import tiny.ast._
import primj.ast.Implicits._
import calcj.ast._
import primj.ast._
import primj.ast.TreeExtractors._

import calcj.ast.operators._
import calcj.types._
import primj.symbols.VariableSymbol
import primj.modifiers.Ops._

trait TreeUtils extends calcj.ast.TreeUtils {
  /** Does the given tree represent a type-use tree */
  def isTypeUse(tree: UseTree): Boolean = tree match {
    case _: IdentApi                    => false
    case _: TypeUseApi                  => true
  }

  /** Does the given tree represent a variable definition */
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

  /** Does the given tree have {{{FINAL}}} flag */
  def isFinal(tree: Tree): Boolean =
    tree.symbol.map(_.mods.isFinal).getOrElse(false)


  /** Is the given tree a valid statement */
  def isValidStatement(e: Tree): Boolean = {
    lazy val isStmt = e match {
      // Statements in primj: if, while, for, block, return, valdef
      // brokenj adds: Switch, continue, break, label
      case _: IfApi | _: WhileApi | _: ForApi | _: BlockApi |
         _: ReturnApi | _: ValDefApi | NoTree     => true
      case _                                      => false
    }
    isValidStatementExpression(e) || isStmt
  }

  /** Is the given tree a valid statement expression */
  def isValidStatementExpression(e: Tree): Boolean = e match {
    case u: UnaryApi  if u.op == Inc || u.op == Dec => true
    case _: ApplyApi                       => true
    // case _: New                         => true
    case _: AssignApi                      => true
    case NoTree                         => true
    case _                              => false
  }

  /** Is the given tree a variable definition or a valid statement expression */
  def isValDefOrStatementExpression(v: Tree): Boolean = v match {
    case s: ValDefApi => true
    case e: Expr   => isValidStatementExpression(e)
    case _         => false
  }

  // INFO: Update this to Java as we go
  /** Is the given tree a valid expression */
  def isValidExpression(e: Tree): Boolean = e match {
    case _: LiteralApi | _: IdentApi | _: BinaryApi | _: UnaryApi |
         _: CastApi | _: AssignApi | _: TernaryApi | _: ApplyApi        => true
    case _                                                              => false
  }

  /** Does the given tree have a return statement in all paths that it can take */
  def allPathsReturn(expr: Tree): Boolean =
    allPathsReturnAux(expr, allPathsReturn)


  /** Does the given tree have a return statement in all paths that it can take */
  protected def allPathsReturnAux(expr: Tree,
          recurse: Tree => Boolean): Boolean = expr match {
    case wile: WhileApi                     =>
      wile.cond match {
        case Literal(Constant(true))        =>
          true
        case _                              =>
          recurse(wile.body)
      }
    case forloop: ForApi                    =>
      forloop.cond match {
        case Literal(Constant(true))        =>
          true
        case _                              =>
          recurse(forloop.body)
      }
    case ifelse: IfApi                      =>
      recurse(ifelse.thenp) &&
      recurse(ifelse.elsep)
    case block: BlockApi                    =>
      block.stmts match {
        case Nil         => false
        case stmts       => recurse(stmts.last)
      }
    case ret: ReturnApi                     =>
      true
    case _                                  =>
      false
  }


  /** Is the given tree a simple expression */
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
    case _                           => true
  }

  /** Is the given tree a constant literal */
  def isConstantLiteral(tree: Tree): Boolean = tree match {
    case lit: LiteralApi              =>
      true
    case _                            =>
      false
  }
}

object TreeUtils extends TreeUtils
