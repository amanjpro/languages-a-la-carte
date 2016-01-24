package ch.usi.inf.l3.sana.ooj.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.brokenj
import sana.ooj
import brokenj.ast
import primj.ast.{ValDefApi, BlockApi}
import tiny.ast.{Tree, NoTree, TypeUseApi, UseTree, IdentApi}
import calcj.ast._
import calcj.types._
import tiny.types.Type
import ooj.types.TypeUtils
import Implicits._
import ooj.symbols.SymbolUtils
import ooj.modifiers.Ops._
import ooj.ast.TreeExtractors._


trait TreeUtils extends ast.TreeUtils {
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

  def isType(tree: Tree): Boolean = tree match {
    case _: TypeUseApi                   => true
    case Select(_, _: TypeUseApi)        => true
    case _                               => false
  }

  def isValidClassMember(tree: Tree): Boolean = tree match {
    case _: MethodDefApi                 => true
    case _: ValDefApi                    => true
    case _: BlockApi                     => true
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


  def isExplicitConstructorInvocation(tree: Tree): Boolean = tree match {
    case Apply(Select(_: ThisApi, id: IdentApi), _)  if isConstructor(id)   =>
      true
    case Apply(Select(_: SuperApi, id: IdentApi), _) if isConstructor(id)   =>
      true
    case _                                                                  =>
      false
  }


  /** Checks if this is an access to a field of the current
   *  instance
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
}

object TreeUtils extends TreeUtils

