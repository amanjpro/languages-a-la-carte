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

  override def isVariable(tree: Tree): Boolean = tree match {
    case Select(_, t)                =>
      isVariable(t)
    case _                           =>
      super.isVariable(tree)
  }
}

object TreeUtils extends TreeUtils

