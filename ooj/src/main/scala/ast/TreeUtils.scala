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

package ch.usi.inf.l3.sana.ooj.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.brokenj
import sana.ooj
import brokenj.ast
import primj.ast.{ValDefApi, BlockApi, WhileApi, ForApi}
import brokenj.ast.BreakApi
import tiny.ast.{Tree, NoTree, TypeUseApi, UseTree, IdentApi}
import ooj.ast.TreeExtractors._
import calcj.ast._
import calcj.types._
import tiny.types.Type
import ooj.names.StdNames.CONSTRUCTOR_NAME
import ooj.types.TypeUtils
import Implicits._
import ooj.symbols.SymbolUtils
import ooj.modifiers.Ops._
import ooj.ast.TreeExtractors._


trait TreeUtils extends ast.TreeUtils {
  /**
   * Checks if a tree represents a constructor. A constructor tree is a {{{MethodDefApi}}}
   * that has the flag {{{CONSTRUCTOR}}} set.
   *
   * @param tree the tree to be checked.
   */
  def isConstructor(tree: Tree): Boolean =
    tree.symbol.map(SymbolUtils.isConstructor(_)) match {
      case Some(v)                                  => v
      case None                                     =>
        tree match {
          case mthd: MethodDefApi =>
            mthd.mods.isConstructor
          case _                  =>
            false
        }
    }


  /**
   * Is this {{{UseTree}}} in the `extends` clause.
   *
   * @param tree the tree to be checked
   */
  def isInExtendsClause(tree: UseTree): Boolean = {
    tree match {
      case Select(_, tuse: TypeUseApi) =>
        tuse.isInExtendsClause
      case tuse: TypeUseApi            =>
        tuse.isInExtendsClause
      case _                           =>
        false
    }
  }

  /**
   * Is this {{{UseTree}}} in the `implemented` clause.
   *
   * @param tree the tree to be checked
   */
  def isInImplementsClause(tree: UseTree): Boolean = {
    tree match {
      case Select(_, tuse: TypeUseApi) =>
        tuse.isInImplementsClause
      case tuse: TypeUseApi            =>
        tuse.isInImplementsClause
      case _                           =>
        false
    }
  }

  /**
   * Does this {{{UseTree}}} point a type or a term
   *
   * @param tree the tree to be checked
   */
  override def isTypeUse(tree: UseTree): Boolean = tree match {
    case _: IdentApi                    => false
    case _: TypeUseApi                  => true
    case Select(_, t)                   =>
      isTypeUse(t)
  }

  // def isType(tree: Tree): Boolean = tree match {
  //   case t: UseTree                      => isTypeUse(t)
  //   case _                               => false
  // }

  /**
   * Checks if a tree is a valid class member.
   * As of Java 1.0, valid class members are: methods, fields and static
   * initializers (blocks)
   *
   * @param tree the tree to be checked
   */
  def isValidClassMember(tree: Tree): Boolean = tree match {
    case _: MethodDefApi                 => true
    case f: ValDefApi                    => f.mods.isField
    case b: BlockApi                     => b.isStaticInit
    case _                               => false
  }

  override def isValidStatementExpression(e: Tree): Boolean = e match {
    case _: NewApi                       => true
    case s: SelectApi                    =>
      isValidStatementExpression(s.qual)
    case _                               =>
      super.isValidStatementExpression(e)
  }

  override def isValidExpression(e: Tree): Boolean = e match {
    case _: NewApi                   => true
    case _: ThisApi                  => true
    case _: SuperApi                 => true
    case _: SelectApi                => true
    case _                           =>
      super.isValidExpression(e)
  }

  override def isSimpleExpression(tree: Tree): Boolean = tree match {
    case _: SelectApi                                   => true
    case _: SuperApi                                    => true
    case _: ThisApi                                     => true
    case _: NewApi                                      => true
    case _                                              =>
      super.isSimpleExpression(tree)
  }


  /**
   * Checks whether this tree represent an explicit constructor invocation
   *
   * @param tree the tree to be checked
   */
  def isExplicitConstructorInvocation(tree: Tree): Boolean = tree match {
    case Apply(Select(_: ThisApi, id: IdentApi), _)
          if id.name == CONSTRUCTOR_NAME                   =>
      true
    case Apply(Select(_: SuperApi, id: IdentApi), _)
          if id.name == CONSTRUCTOR_NAME                   =>
      true
    case _                                                 =>
      false
  }

  override def allPathsReturn(expr: Tree): Boolean =
    allPathsReturnAux(expr, allPathsReturn)

  override protected def allPathsReturnAux(expr: Tree,
          recurse: Tree => Boolean): Boolean = expr match {
    case wile: WhileApi                      =>
      wile.cond match {
        case Literal(Constant(true))         =>
          wile.body match {
            case b: BlockApi                 =>
              !b.stmts.exists {
                case s: BreakApi             =>
                  true
                case s   if isBreakable(s)   =>
                  !recurse(s)
                case _                       =>
                  false
              }
            case _                           =>
              false
          }
        case _                               =>
          recurse(wile.body)
      }
    case forloop: ForApi                     =>
      forloop.cond match {
        case Literal(Constant(true))         =>
          forloop.body match {
            case b: BlockApi                 =>
              !b.stmts.exists {
                case s: BreakApi             =>
                  true
                case s   if isBreakable(s)   =>
                  !recurse(s)
                case _                       =>
                  false
              }
            case _                           =>
              false
          }
        case _                               =>
          recurse(forloop.body)
      }
    case _                                   =>
      super.allPathsReturnAux(expr, recurse)
  }
  /**
   * Checks if this is an access to a field of the current instance
   *
   * @param tree the tree to be checked
   */
  def isThisFieldAccess(tree: Tree): Boolean = tree match {
    case id: IdentApi             =>
      id.symbol.map(_.mods.isField).getOrElse(false)
    case Select(_: ThisApi, id)   =>
      isThisFieldAccess(id)
    case Select(_: SuperApi, id)  =>
      isThisFieldAccess(id)
    case _                        =>
      false
  }


  override def isConstantLiteral(tree: Tree): Boolean = tree match {
    case Literal(constant)  =>
      constant.tpe.isInstanceOf[PrimitiveType] ||
        constant.tpe =:= TypeUtils.stringClassType
    case _                  =>
      false

  }


  /**
   * Returns the default value of a field, based on its type. The default
   * value is returned as per Java specification.
   *
   * @param tpe the type of the field of which we want its default value.
   */
  def getDefaultFieldValue(tpe: Option[Type]): Tree = tpe match {
    case Some(ByteType)    =>
      TreeFactories.mkLiteral(ByteConstant(0))
    case Some(ShortType)   =>
      TreeFactories.mkLiteral(ShortConstant(0))
    case Some(CharType)    =>
      TreeFactories.mkLiteral(CharConstant('\u0000'))
    case Some(IntType)     =>
      TreeFactories.mkLiteral(IntConstant(0))
    case Some(LongType)    =>
      TreeFactories.mkLiteral(LongConstant(0L))
    case Some(FloatType)   =>
      TreeFactories.mkLiteral(FloatConstant(0.0F))
    case Some(DoubleType)  =>
      TreeFactories.mkLiteral(DoubleConstant(0))
    case Some(BooleanType) =>
      TreeFactories.mkLiteral(BooleanConstant(false))
    case _                 =>
      TreeFactories.mkLiteral(NullConstant)
  }

  override def isVariable(tree: Tree): Boolean = tree match {
    case Select(_, t)                =>
      isVariable(t)
    case _                           =>
      super.isVariable(tree)
  }
}

object TreeUtils extends TreeUtils
