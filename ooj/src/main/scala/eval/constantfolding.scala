package ch.usi.inf.l3.sana.ooj.eval

import ch.usi.inf.l3.sana
import sana.ooj
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj

import sana.core.TransformationComponent
import sana.dsl._


import tiny.types.Type
import tiny.symbols.Symbol
import ooj.ast._
import ooj.ast.Implicits._
import ooj.ast.TreeExtractors._
import ooj.modifiers.Ops._
import calcj.ast.operators._
import calcj.types._
import calcj.typechecker.TypePromotions
import ooj.symbols.{SymbolUtils, PackageSymbol}
import ooj.types.TypeUtils
import ooj.ast.Implicits._
import brokenj.ast.{TreeCopiers => _, TreeUtils => _, _}
import primj.ast.{TreeCopiers => _, MethodDefApi => _,
                  ProgramApi => _, TreeUtils => _, _}
import calcj.ast.{TreeCopiers => _, _}
import tiny.ast.{TreeCopiers => _, _}



trait ConstantFoldingComponent extends
  TransformationComponent[(Tree, Env), (Tree, Env)] {
  def constantFold: ((Tree, Env)) => (Tree, Env)
}

@component(tree, env)
trait ProgramConstantFoldingComponent extends ConstantFoldingComponent {
  (prg: ProgramApi)  => {
    val (members, newEnv) = prg.members.foldLeft((Nil: List[Tree], env)){
      (z, member) =>
        val e       = z._2
        val members = z._1
        val (res, env) = constantFold((member, e))
        (members++List(res), env)

    }
    val res = TreeCopiers.copyProgram(prg)(members = members)
    res.bottomUp(())((_, y) => y match {
      case v: ValDefApi    if v.mods.isFinal && !v.mods.isField &&
                             v.rhs != NoTree    =>
        for {
          s <- v.symbol
          o <- s.owner
        } {
          o.delete(s)
        }
      case _                                    =>
        ()
    })
    (res, newEnv)
  }
}

@component(tree, env)
trait CompilationUnitConstantFoldingComponent extends
    ConstantFoldingComponent {
  (cunit: CompilationUnitApi)  => {
    val (module, newEnv) = constantFold((cunit.module, env))
    (TreeCopiers.copyCompilationUnit(cunit)(module =
        module.asInstanceOf[PackageDefApi]), newEnv)
  }
}

@component(tree, env)
trait PackageDefConstantFoldingComponent
  extends ConstantFoldingComponent {
  (pkg: PackageDefApi)  => {
    val (members, newEnv) = pkg.members.foldLeft((Nil: List[Tree], env)){
      (z, member) =>
        val e       = z._2
        val members = z._1
        val (res, env) = constantFold((member, e))
        (members++List(res), env)

    }
    (TreeCopiers.copyPackageDef(pkg)(members = members), newEnv)
  }
}

@component(tree, env)
trait ClassDefConstantFoldingComponent
  extends ConstantFoldingComponent {
  (clazz: ClassDefApi)  => {
    val (body, newEnv) = constantFold((clazz.body, env))
    (TreeCopiers.copyClassDef(clazz)(body = body.asInstanceOf[TemplateApi]),
      newEnv)
  }
}

@component(tree, env)
trait TemplateConstantFoldingComponent
  extends ConstantFoldingComponent {
  (template: TemplateApi)  => {
    val (members, newEnv) = template.members.foldLeft((Nil: List[Tree], env)){
    (z, member) =>
      val e       = z._2
      val members = z._1
      val (res, env) = constantFold((member, e))
      (members++List(res), env)
    }
    (TreeCopiers.copyTemplate(template)(members = members), newEnv)
  }
}


@component(tree, env)
trait ValDefConstantFoldingComponent
  extends ConstantFoldingComponent {
  (valdef: ValDefApi) => {
    if(valdef.rhs != NoTree) {
      if(valdef.mods.isFinal && !valdef.mods.isField) {
        valdef.owner.foreach(sym => {
          valdef.symbol.foreach(sym.declare(_))
        })
      }
      val (tpt, _) = constantFold((valdef.tpt, env))
      val vtpe     = tpt.symbol.flatMap(_.tpe)
      valdef.symbol.map { sym =>
        env.getValue(sym) match {
          case Some(LiteralValue(lit))       =>
            val rhs = vtpe.map(updateLiteral(lit, _)).getOrElse(lit)
            (updateRhs(valdef, rhs), env)
          case Some(ExprValue(expr))         =>
            foldRhs(valdef, expr, env, sym, vtpe)
          case None if !valdef.mods.isField  =>
            foldRhs(valdef, valdef.rhs, env, sym, vtpe)
          case _                             =>
            (valdef, env)
        }
      }.getOrElse((valdef, env))
    } else {
     (valdef, env)
    }
  }

  protected def foldRhs(valdef: ValDefApi,
        expr: Expr, env: Env, sym: Symbol,
        tpe: Option[Type]): (ValDefApi, Env) = {
    val (rhs, env2) = constantFold((expr, env))
    rhs match {
      case lit: LiteralApi       =>
        val rhs = tpe.map(updateLiteral(lit, _)).getOrElse(lit)
        val e = valdef.mods.isFinal match {
          case true => env2.bind(sym, LiteralValue(rhs))
          case false => env2
        }
        (updateRhs(valdef, rhs), e)
      case rhs: Expr             =>
        val e = env2.unbind(sym)
        (updateRhs(valdef, rhs), e)
      case _                     =>
        val e = env2.unbind(sym)
        (valdef, e)
    }
  }

  protected def updateLiteral(lit: LiteralApi, tpe: Type): LiteralApi = {
    if(isNarrawableTo(lit, tpe)) {
      narrowDown(lit, tpe)
    } else if(lit.constant.tpe <:< tpe) {
      widen(lit, tpe)
    } else {
      lit
    }
  }

  protected def updateRhs(valdef: ValDefApi, rhs: Expr): ValDefApi = {
    TreeCopiers.copyValDef(valdef)(rhs = rhs)
  }

  protected def narrowDown(lit: LiteralApi, tpe: Type): LiteralApi =
    TreeUtils.narrowDown(lit, tpe)
  protected def widen(lit: LiteralApi, tpe: Type): LiteralApi =
    TreeUtils.widen(lit, tpe)
  protected def isNarrawableTo(e: Tree, t: Type): Boolean =
    TypePromotions.isNarrawableTo(e, t)

}



@component(tree, env)
trait MethodDefConstantFoldingComponent
  extends ConstantFoldingComponent {
  (mthd: MethodDefApi) => {
    val (body, newEnv) = constantFold((mthd.body, env))
    (TreeCopiers.copyMethodDef(mthd)(body = body.asInstanceOf[Expr]), newEnv)
  }
}

@component(tree, env)
trait BlockConstantFoldingComponent
  extends ConstantFoldingComponent {
  (block: BlockApi) => {
    val (stmts, newEnv) = block.stmts.foldLeft((Nil: List[Tree], env)){
      (z, stmt) =>
        val e       = z._2
        val stmts   = z._1
        val (res, e2) = constantFold((stmt, e))
        (stmts++List(res), e2)
    }
    (TreeCopiers.copyBlock(block)(stmts = stmts), newEnv)
  }
}

@component(tree, env)
trait SelectConstantFoldingComponent
  extends ConstantFoldingComponent {
  (slct: SelectApi) => {
    val select = TreeCopiers.copySelect(slct)()
    val (qual, _) = constantFold((select.qual, env))
    qual.symbol.foreach(select.tree.owner = _)
    val tree = if(isTypeSymbol(qual.symbol) ||
                       isPackageSymbol(qual.symbol)) {
                      val (t, _) = constantFold((select.tree, env))
                      t
                    } else select.tree

    if(isTypeSymbol(qual.symbol)) {
      tree match {
        case tree: SimpleUseTree =>
          tree.shouldBeStatic = true
        case _                   =>
      }
    }
    if(isTypeSymbol(qual.symbol) &&
        isConstantLiteral(tree)) {
      (tree, env)
    } else if(isTypeSymbol(qual.symbol) &&
        isStaticFinal(tree.symbol)) {
      tree.symbol.map { sym =>
        env.getValue(sym) match {
          case Some(LiteralValue(l))           =>
            (l, env)
          case Some(ExprValue(e))              =>
            constantFold((e, env)) match {
              case (l@Literal(c), env)         =>
                if(c.tpe =:= stringClassType ||
                    c.tpe.isInstanceOf[PrimitiveType])
                  (l, env)
                else
                  (slct, env)
              case (_, env)                    =>
                (slct, env)
            }
          case _                               =>
            (slct, env)
        }
      }.getOrElse((slct, env))
    } else if(isPackageSymbol(qual.symbol) &&
             (isTypeSymbol(tree.symbol))) {
      (TreeCopiers.copySelect(slct)(qual = qual,
                      tree = tree.asInstanceOf[SimpleUseTree]), env)
    } else (slct, env)
  }

  protected def isConstantLiteral(tree: Tree): Boolean =
    TreeUtils.isConstantLiteral(tree)

  protected def isPackageSymbol(sym: Option[Symbol]): Boolean =
    sym.map(_.isInstanceOf[PackageSymbol]).getOrElse(false)

  protected val stringClassType: Type = TypeUtils.stringClassType

  def isTypeSymbol(sym: Option[Symbol]): Boolean =
    SymbolUtils.isTypeSymbol(sym)

  def isStaticFinal(sym: Option[Symbol]): Boolean = sym.map { s =>
    s.mods.isStatic && s.mods.isFinal
  }.getOrElse(false)
}

@component(tree, env)
trait TypeUseConstantFoldingComponent
  extends ConstantFoldingComponent
  with ooj.typechecker.TypeUseNamer {
  (tuse: TypeUseApi) => {
    (nameTypeUse(tuse), env)
  }
}

@component(tree, env)
trait IdentConstantFoldingComponent
  extends ConstantFoldingComponent
  with ooj.namers.IdentNamer
  with ooj.typechecker.IdentNamer {
  (id: IdentApi)    => {
    nameIdent(id) match {
      case id: IdentApi            =>
        val ident = nameIdent(id, false)
        ident.symbol.map { sym =>
          env.getValue(sym) match {
            case Some(ExprValue(expr))                    =>
              val (v, env2) = constantFold((expr, env))
              v match {
                case lit: LiteralApi          =>
                  val clit =
                    TreeCopiers.copyLiteral(lit)(constant = lit.constant)
                  ident.pos.foreach(clit.pos = _)
                  (clit, env2.bind(sym, LiteralValue(lit)))
                case _                        =>
                  (ident, env)
              }
            case Some(LiteralValue(lit))                  =>
              val clit = TreeCopiers.copyLiteral(lit)(constant = lit.constant)
              ident.pos.foreach(clit.pos = _)
              (clit, env)
            case None if sym.isInstanceOf[PackageSymbol]  =>
              (id, env)
            case _                     =>
              (ident, env)
          }
        }.getOrElse((ident, env))
      case tuse: TypeUse            =>
        (tuse, env)
      case _                        =>
        (id, env)
    }
  }
}

@component(tree, env)
trait BinaryConstantFoldingComponent
  extends ConstantFoldingComponent {
  (bin: BinaryApi) => {
    val (lhs, env1) = constantFold((bin.lhs, env))
    val (rhs, env2) = constantFold((bin.rhs, env1))
    (lhs, rhs) match {
      case (lit@Literal(l1), Literal(l2))               =>
        bin.op match {
          case Sub | Mul | Div | Mod | Add         =>
            if(l1.tpe =:= stringClassType && l2.tpe =:= stringClassType &&
              bin.op == Add) {
              val res = TreeCopiers.copyLiteral(lit)(
                constant = StringConstant (
                  l1.value.asInstanceOf[String] +
                  l2.value.asInstanceOf[String]))
              (res, env2)
            } else if(!(l1.tpe =:= stringClassType) &&
              l2.tpe =:= stringClassType &&
              bin.op == Add) {
              val res = TreeCopiers.copyLiteral(lit)(
                constant = StringConstant (
                  l1.value.toString +
                  l2.value.asInstanceOf[String]))
              (res, env2)
            } else if(l1.tpe =:= stringClassType &&
              !(l2.tpe =:= stringClassType) &&
              bin.op == Add) {
              val res = TreeCopiers.copyLiteral(lit)(
                constant = StringConstant (
                  l1.value.asInstanceOf[String] +
                  l2.value.toString))
              (res, env2)
            } else if(l1.tpe <:< IntType && l2.tpe <:< IntType) {
              val res = TreeCopiers.copyLiteral(lit)(
                constant = IntConstant (
                  bop2BinaryInt(bin.op)(
                  l1.value.asInstanceOf[Int], l2.value.asInstanceOf[Int])))
              (res, env2)
            } else if(l1.tpe.isInstanceOf[NumericType] &&
                      l2.tpe.isInstanceOf[NumericType]) {
              if(l1.tpe.isInstanceOf[IntegralType] &&
                 l2.tpe.isInstanceOf[IntegralType]) {
                val res = TreeCopiers.copyLiteral(lit)(
                  constant = LongConstant(
                    bop2BinaryLong(bin.op)(
                    l1.value.asInstanceOf[Int], l2.value.asInstanceOf[Int])))
                (res, env2)
              } else if(l1.tpe <:< FloatType && l2.tpe <:< FloatType) {
                val res = TreeCopiers.copyLiteral(lit)(
                  constant = FloatConstant (
                    bop2BinaryFloat(bin.op)(
                    l1.value.asInstanceOf[Int], l2.value.asInstanceOf[Int])))
                (res, env2)
              } else if(l1.tpe <:< DoubleType || l2.tpe <:< DoubleType) {
                val res = TreeCopiers.copyLiteral(lit)(
                  constant = DoubleConstant (
                    bop2BinaryDouble(bin.op)(
                    l1.value.asInstanceOf[Int], l2.value.asInstanceOf[Int])))
                (res, env2)
              } else (bin, env2)
            } else {
              (bin, env2)
            }
          case SHL | SHR | USHR                    =>
            if(l1.tpe.isInstanceOf[IntegralType] ||
               l2.tpe.isInstanceOf[IntegralType]) {
              def fIntInt: (Int, Int) => Int = bin.op match {
                case SHL   => _ << _
                case SHR   => _ >> _
                case USHR  => _ >>> _
              }
              def fIntLong: (Int, Long) => Int = bin.op match {
                case SHL   => _ << _
                case SHR   => _ >> _
                case USHR  => _ >>> _
              }
              def fLongInt: (Long, Int) => Long = bin.op match {
                case SHL   => _ << _
                case SHR   => _ >> _
                case USHR  => _ >>> _
              }
              def fLongLong: (Long, Long) => Long = bin.op match {
                case SHL   => _ << _
                case SHR   => _ >> _
                case USHR  => _ >>> _
              }
              val c2v = (c: Constant) => {
                if(c.tpe =:= LongType)       c.value.asInstanceOf[Long]
                else                         c.value.asInstanceOf[Int]
              }

              val lhs = c2v(l1)
              val rhs = c2v(l2)
              val const = if(l1.tpe =:= LongType) {
                if(l2.tpe =:= LongType)
                  LongConstant(fLongLong(
                    lhs.asInstanceOf[Long], rhs.asInstanceOf[Long]))
                else
                  LongConstant(fLongInt(
                    lhs.asInstanceOf[Long], rhs.asInstanceOf[Int]))
              } else {
                if(l2.tpe =:= LongType)
                  IntConstant(fIntLong(
                    lhs.asInstanceOf[Int], rhs.asInstanceOf[Long]))
                else
                  IntConstant(fIntInt(
                    lhs.asInstanceOf[Int], rhs.asInstanceOf[Int]))
              }
              val res = TreeCopiers.copyLiteral(lit)(constant = const)
              (res, env2)
            } else {
              (bin, env2)
            }
          case Lt | Gt | Le | Ge                   =>
            val relInt: (Int, Int) => Boolean = bin.op match {
              case Lt    => _ < _
              case Gt    => _ > _
              case Le    => _ <= _
              case Ge    => _ >= _
            }
            val relLong: (Long, Long) => Boolean = bin.op match {
              case Lt    => _ < _
              case Gt    => _ > _
              case Le    => _ <= _
              case Ge    => _ >= _
            }
            val relFloat: (Float, Float) => Boolean = bin.op match {
              case Lt    => _ < _
              case Gt    => _ > _
              case Le    => _ <= _
              case Ge    => _ >= _
            }
            val relDouble: (Double, Double) => Boolean = bin.op match {
              case Lt    => _ < _
              case Gt    => _ > _
              case Le    => _ <= _
              case Ge    => _ >= _
            }
            if(l1.tpe.isInstanceOf[NumericType] &&
               l2.tpe.isInstanceOf[NumericType]) {
              val const = if(l1.tpe <:< IntType &&
                 l2.tpe <:< IntType) {
                BooleanConstant (
                  relInt(l1.value.asInstanceOf[Int],
                           l2.value.asInstanceOf[Int]))
              } else if(l1.tpe.isInstanceOf[IntegralType] &&
                 l2.tpe.isInstanceOf[IntegralType]) {
                BooleanConstant (
                  relLong(l1.value.asInstanceOf[Long],
                           l2.value.asInstanceOf[Long]))
              } else if(l1.tpe <:< FloatType &&
                 l2.tpe <:< FloatType) {
                BooleanConstant (
                  relFloat(l1.value.asInstanceOf[Float],
                           l2.value.asInstanceOf[Float]))
              } else {
                BooleanConstant (
                  relDouble(l1.value.asInstanceOf[Double],
                            l2.value.asInstanceOf[Double]))
              }
              val res = TreeCopiers.copyLiteral(lit)(constant = const)
              (res, env2)
            } else (bin, env2)
          case Eq | Neq                            =>
            val eqStr: (String, String) => Boolean = bin.op match {
              case Eq     => _ == _
              case Neq    => _ != _
            }
            val eqAnyVal: (AnyVal, AnyVal) => Boolean = bin.op match {
              case Eq     => _ == _
              case Neq    => _ != _
            }
            if(l1.tpe <:< BooleanType &&
               l2.tpe <:< BooleanType) {
              val res = TreeCopiers.copyLiteral(lit)(
                constant = BooleanConstant (
                  eqAnyVal(l1.value.asInstanceOf[Boolean],
                           l2.value.asInstanceOf[Boolean])))
              (res, env2)
            } else if(l1.tpe.isInstanceOf[NumericType] &&
               l2.tpe.isInstanceOf[NumericType]) {
              val const = if(l1.tpe <:< IntType &&
                 l2.tpe <:< IntType) {
                BooleanConstant (
                  eqAnyVal(l1.value.asInstanceOf[Int],
                           l2.value.asInstanceOf[Int]))
              } else if(l1.tpe.isInstanceOf[IntegralType] &&
                 l2.tpe.isInstanceOf[IntegralType]) {
                BooleanConstant (
                  eqAnyVal(l1.value.asInstanceOf[Long],
                           l2.value.asInstanceOf[Long]))
              } else if(l1.tpe <:< FloatType &&
                 l2.tpe <:< FloatType) {
                BooleanConstant (
                  eqAnyVal(l1.value.asInstanceOf[Float],
                           l2.value.asInstanceOf[Float]))
              } else {
                BooleanConstant (
                  eqAnyVal(l1.value.asInstanceOf[Double],
                           l2.value.asInstanceOf[Double]))
              }
              val res = TreeCopiers.copyLiteral(lit)(constant = const)
              (res, env2)
            } else if(l1.tpe =:= stringClassType &&
                      l2.tpe =:= stringClassType) {
              val res = TreeCopiers.copyLiteral(lit)(
                constant = BooleanConstant (
                  eqStr(l1.value.asInstanceOf[String],
                        l2.value.asInstanceOf[String])))
              (res, env2)
            } else (bin, env2)
          case BAnd | BOr | BXor                   =>
            val bitwiseInt: (Int, Int) => Int = bin.op match {
              case BAnd => _ & _
              case BOr  => _ | _
              case BXor => _ | _
            }
            val bitwiseLong: (Long, Long) => Long = bin.op match {
              case BAnd => _ & _
              case BOr  => _ | _
              case BXor => _ | _
            }
            val bitwiseBoolean: (Boolean, Boolean) => Boolean = bin.op match {
              case BAnd => _ & _
              case BOr  => _ | _
              case BXor => _ | _
            }
            if (l1.tpe.isInstanceOf[IntegralType] &&
                l2.tpe.isInstanceOf[IntegralType]) {
              val cnst = if(l1.tpe <:< BooleanType &&
                 l2.tpe <:< BooleanType) {
                IntConstant(
                  bitwiseInt(l1.value.asInstanceOf[Int],
                             l2.value.asInstanceOf[Int]))
              } else {
                LongConstant(
                  bitwiseLong(l1.value.asInstanceOf[Long],
                             l2.value.asInstanceOf[Long]))
              }
              val res = TreeCopiers.copyLiteral(lit)(constant = cnst)
              (res, env2)
            } else if(l1.tpe <:< BooleanType &&
                      l2.tpe <:< BooleanType) {
              val res = TreeCopiers.copyLiteral(lit)(
                constant = BooleanConstant (
                  bitwiseBoolean(l1.value.asInstanceOf[Boolean],
                                 l2.value.asInstanceOf[Boolean])))
              (res, env2)
            } else (bin, env2)
          case And | Or                            =>
            val f: (Boolean, Boolean) => Boolean = bin.op match {
              case And   => _ && _
              case Or    => _ || _
            }
            if(l1.tpe <:< BooleanType && l2.tpe <:< BooleanType) {
              val res = TreeCopiers.copyLiteral(lit)(
                constant = BooleanConstant (
                  f(l1.value.asInstanceOf[Boolean],
                    l2.value.asInstanceOf[Boolean])))
              (res, env2)
            } else (bin, env2)
          case _                                   =>
            (bin, env2)
        }
      case _                                            =>
        (bin, env2)
    }
  }

  protected def bop2BinaryInt(op: BOp): (Int, Int) => Int = op match {
    case Add     => _ + _
    case Sub     => _ - _
    case Mul     => _ * _
    case Div     => _ / _
    case Mod     => _ % _
  }

  protected def bop2BinaryLong(op: BOp): (Long, Long) => Long = op match {
    case Add     => _ + _
    case Sub     => _ - _
    case Mul     => _ * _
    case Div     => _ / _
    case Mod     => _ % _
  }

  protected def bop2BinaryFloat(op: BOp): (Float, Float) => Float = op match {
    case Add     => _ + _
    case Sub     => _ - _
    case Mul     => _ * _
    case Div     => _ / _
    case Mod     => _ % _
  }

  protected def bop2BinaryDouble(op: BOp
        ): (Double, Double) => Double = op match {
    case Add     => _ + _
    case Sub     => _ - _
    case Mul     => _ * _
    case Div     => _ / _
    case Mod     => _ % _
  }

  protected val stringClassType: Type = TypeUtils.stringClassType
}

@component(tree, env)
trait UnaryConstantFoldingComponent
  extends ConstantFoldingComponent {
  (unary: UnaryApi) => {
    val (expr, env1) = constantFold((unary.expr, env))
    expr match {
      case lit@Literal(l) =>
        unary.op match {
          case Not    if l.tpe =:= BooleanType              =>
            val res = TreeCopiers.copyLiteral(lit)(
                constant = BooleanConstant (!l.value.asInstanceOf[Boolean]))
            (res, env1)
          case Pos    if l.tpe.isInstanceOf[NumericType]    =>
            val const = if(l.tpe <:< IntType) {
              IntConstant(l.value.asInstanceOf[Int])
            } else if(l.tpe =:= LongType) {
              LongConstant(l.value.asInstanceOf[Long])
            } else if(l.tpe =:= FloatType) {
              FloatConstant(l.value.asInstanceOf[Float])
            } else {
              DoubleConstant(l.value.asInstanceOf[Double])
            }
            val res = TreeCopiers.copyLiteral(lit)(constant = const)
            (res, env1)
          case Neg    if l.tpe.isInstanceOf[NumericType]    =>
            val const = if(l.tpe <:< IntType) {
              IntConstant(- l.value.asInstanceOf[Int])
            } else if(l.tpe =:= LongType) {
              LongConstant(- l.value.asInstanceOf[Long])
            } else if(l.tpe =:= FloatType) {
              FloatConstant(- l.value.asInstanceOf[Float])
            } else {
              DoubleConstant(- l.value.asInstanceOf[Double])
            }
            val res = TreeCopiers.copyLiteral(lit)(constant = const)
            (res, env1)
          case BCompl if l.tpe.isInstanceOf[IntegralType]   =>
            val const = if(l.tpe <:< IntType) {
              IntConstant(~ l.value.asInstanceOf[Int])
            } else {
              LongConstant(~ l.value.asInstanceOf[Long])
            }
            val res = TreeCopiers.copyLiteral(lit)(constant = const)
            (res, env1)
          case _                                            =>
            (unary, env1)
        }
      case _          =>
        (unary, env1)
    }
  }
}


@component(tree, env)
trait TernaryConstantFoldingComponent
  extends ConstantFoldingComponent {
  (tern: TernaryApi) => {
    val (cond,  env1) = constantFold((tern.cond,  env))
    val (thenp, env2) = constantFold((tern.thenp, env1))
    val (elsep, env3) = constantFold((tern.elsep, env2))
    val res = cond match {
      case Literal(BooleanConstant(true))  =>
        thenp
      case Literal(BooleanConstant(false)) =>
        elsep
      case _                               =>
        TreeCopiers.copyTernary(tern)(cond = cond.asInstanceOf[Expr],
        thenp = thenp.asInstanceOf[Expr],
        elsep = elsep.asInstanceOf[Expr])
    }
    (res, env3)
  }
}

@component(tree, env)
trait CastConstantFoldingComponent
  extends ConstantFoldingComponent {
  (cst: CastApi) => {
    val (newTpt, _)       = constantFold((cst.tpt, env))
    val (newExpr, newEnv) = constantFold((cst.expr, env))
    val newTree = newExpr match {
      case lit@Literal(c)          =>
        val res = for {
          sym  <- newTpt.symbol
          stpe <- sym.tpe
        } yield {
          if(stpe.isInstanceOf[PrimitiveType] &&
             c.tpe.isInstanceOf[PrimitiveType]) {
            if(isNarrawableTo(lit, stpe)) {
              narrowDown(lit, stpe)
            } else if(c.tpe <:< stpe) {
              widen(lit, stpe)
            } else
              cst
          } else if(stpe =:= stringClassType &&
                    c.tpe =:= stringClassType) {
            lit
          } else {
            cst
          }
        }
        res.getOrElse(cst)
      case _                      =>
        TreeCopiers.copyCast(cst)(expr = newExpr.asInstanceOf[Expr])
    }
    (newTree, newEnv)
  }

  protected def narrowDown(lit: LiteralApi, tpe: Type): LiteralApi =
    TreeUtils.narrowDown(lit, tpe)
  protected def widen(lit: LiteralApi, tpe: Type): LiteralApi =
    TreeUtils.widen(lit, tpe)
  protected val stringClassType: Type = TypeUtils.stringClassType
  protected def isNarrawableTo(e: Tree, t: Type): Boolean =
    TypePromotions.isNarrawableTo(e, t)
}

// boring cases

@component(tree, env)
trait AssignConstantFoldingComponent
  extends ConstantFoldingComponent {
  (assign: AssignApi) => {
    val (rhs, newEnv) = constantFold((assign.rhs, env))
    (TreeCopiers.copyAssign(assign)(rhs = rhs.asInstanceOf[Expr]),
      newEnv)
  }
}

@component(tree, env)
trait ForConstantFoldingComponent
  extends ConstantFoldingComponent {
  (forloop: ForApi) => {
    val (inits, env1)  = forloop.inits.foldLeft((Nil: List[Tree], env)){
      (z, init) =>
        val inits   = z._1
        val e       = z._2
        val (res, env) = constantFold((init, e))
        (inits++List(res), env)
    }
    val (cond, env2)   = constantFold((forloop.cond, env1))
    val (steps, env3)  = forloop.steps.foldLeft((Nil: List[Expr], env2)){
      (z, step) =>
        val steps   = z._1
        val e       = z._2
        val (res, env) = constantFold((step, e))
        (steps++List(step.asInstanceOf[Expr]), env)
    }
    val (body, env4)   = constantFold((forloop.body, env3))
    val newFor = TreeCopiers.copyFor(forloop)(inits = inits,
      cond = cond.asInstanceOf[Expr],
      steps = steps,
      body = body.asInstanceOf[Expr])
    ((newFor, env3))
  }
}

@component(tree, env)
trait ApplyConstantFoldingComponent
  extends ConstantFoldingComponent {
  (app: ApplyApi) => {
    val (args, env1)  = app.args.foldLeft((Nil: List[Expr], env)){
      (z, arg) =>
        val args    = z._1
        val e       = z._2
        val (res, env) = constantFold((arg, e))
        (args++List(res.asInstanceOf[Expr]), env)
    }
    val newApply = TreeCopiers.copyApply(app)(args = args)
    ((newApply, env1))
  }
}

@component(tree, env)
trait IfConstantFoldingComponent
  extends ConstantFoldingComponent {
  (ifelse: IfApi) => {
    val (cond,  env1) = constantFold((ifelse.cond,  env))
    val (thenp, env2) = constantFold((ifelse.thenp, env1))
    val (elsep, env3) = constantFold((ifelse.elsep, env2))
    (TreeCopiers.copyIf(ifelse)(cond = cond.asInstanceOf[Expr],
      thenp = thenp.asInstanceOf[Expr],
      elsep = elsep.asInstanceOf[Expr]),
      env3)
  }
}

@component(tree, env)
trait WhileConstantFoldingComponent
  extends ConstantFoldingComponent {
  (wile: WhileApi) => {
    val (cond,  env1) = constantFold((wile.cond,  env))
    val (body, env2) = constantFold((wile.body, env1))
    (TreeCopiers.copyWhile(wile)(cond = cond.asInstanceOf[Expr],
      body = body.asInstanceOf[Expr]),
      env2)
  }
}

@component(tree, env)
trait SwitchConstantFoldingComponent
  extends ConstantFoldingComponent {
  (switch: SwitchApi) => {
    val (expr, env2)   = constantFold((switch.expr, env))
    val (cases, env3) = switch.cases.foldLeft((Nil: List[CaseApi], env2)){
      (z, cse) =>
        val cases   = z._1
        val e       = z._2
        val (res, e1) = constantFold((cse, e))
        (cases++List(res.asInstanceOf[CaseApi]), e1)
    }
    val newSwitch = TreeCopiers.copySwitch(switch)(cases = cases,
      expr = expr.asInstanceOf[Expr])
    ((newSwitch, env3))
  }
}

@component(tree, env)
trait CaseConstantFoldingComponent
  extends ConstantFoldingComponent {
  (cse: CaseApi) => {
    val (guards, env2) = cse.guards.foldLeft((Nil: List[Expr], env)){
      (z, guard) =>
        val guards   = z._1
        val e       = z._2
        val (res, e2) = constantFold((guard, e))
        (guards++List(res.asInstanceOf[Expr]), e2)
    }
    val (body, env3)  = constantFold((cse.body, env2))
    val newCse = TreeCopiers.copyCase(cse)(guards = guards,
      body = body)
    ((newCse, env3))
  }
}

@component(tree, env)
trait ReturnConstantFoldingComponent
  extends ConstantFoldingComponent {
  (ret: ReturnApi) => {
    ret.expr.map { expr =>
      val (newExpr, newEnv) = constantFold((expr, env))
      val newRet = TreeCopiers.copyReturn(ret)(
        expr = Some(newExpr.asInstanceOf[Expr]))
      (newRet, newEnv)
    }.getOrElse((ret, env))
  }
}

@component(tree, env)
trait LabelConstantFoldingComponent
  extends ConstantFoldingComponent {
  (lbl: LabelApi) => {
    val (newStmt, newEnv) = constantFold((lbl.stmt, env))
    val newLbl = TreeCopiers.copyLabel(lbl)(stmt = newStmt.asInstanceOf[Expr])
    (newLbl, newEnv)
  }
}



@component(tree, env)
trait NewConstantFoldingComponent
  extends ConstantFoldingComponent {
  (nu: NewApi) => {
    val (newApp, newEnv) = constantFold((nu.app, env))
    val newNu = TreeCopiers.copyNew(nu)(app = newApp.asInstanceOf[ApplyApi])
    (newNu, newEnv)
  }
}

@component(tree, env)
trait SuperConstantFoldingComponent
  extends ConstantFoldingComponent {
  (spr: SuperApi) => ((spr, env))
}

@component(tree, env)
trait ThisConstantFoldingComponent
  extends ConstantFoldingComponent {
  (ths: ThisApi) => ((ths, env))
}

@component(tree, env)
trait LiteralConstantFoldingComponent
  extends ConstantFoldingComponent {
  (lit: LiteralApi) => ((lit, env))
}

@component(tree, env)
trait ContinueConstantFoldingComponent
  extends ConstantFoldingComponent {
  (cntnu: ContinueApi) => ((cntnu, env))
}

@component(tree, env)
trait BreakConstantFoldingComponent
  extends ConstantFoldingComponent {
  (brk: BreakApi) => ((brk, env))
}


