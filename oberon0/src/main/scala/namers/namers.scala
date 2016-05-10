package ch.usi.inf.l3.sana.oberon0.namers

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.brokenj
import sana.arrayj
import sana.ooj
import sana.oberon0

import tiny.dsl._
import oberon0.ast._
import tiny.ast.{DefTree, UseTree, Expr, Tree, SimpleUseTree}
import tiny.names.Name
import tiny.source.Position
import tiny.symbols.{Symbol, TypeSymbol}
import ooj.symbols.ClassSymbol
import ooj.ast.{ClassDefApi, TemplateApi, SelectApi}
import calcj.ast.{UnaryApi, LiteralApi, BinaryApi}
import tiny.errors.ErrorReporting.{error, warning}
import oberon0.errors.ErrorCodes._
import primj.ast.{BlockApi, ValDefApi, IfApi, AssignApi, ApplyApi, WhileApi}
import arrayj.ast.ArrayAccessApi
import oberon0.ast.Implicits._
import primj.symbols.VariableSymbol
import oberon0.symbols._
import oberon0.types._
import primj.namers.NamerComponent
import oberon0.symbols.SymbolUtils


// Program: DONE
// ModuleDef: DONE
// ClassDef: DONE
// Template: DONE
// TypeDef: DONE
// ArrayTypeUse: DONE
// TypeUse: DONE
// Ident: DONE
// MethodDef: DONE
// ValDef: DONE
// Apply: DONE
// If: DONE
// While: DONE
// Block: DONE
// Assign: DONE
// ArrayAccess: DONE
// Select: DONE
// Binary: DONE
// Unary: DONE
// Literal: DONE



@component
trait ModuleDefNamerComponent extends NamerComponent {
  (module: ModuleDefApi) => {
    val declarations = module.declarations.map(name(_).asInstanceOf[DefTree])
    val block        = module.block.map(name(_).asInstanceOf[BlockApi])
    TreeCopiers.copyModuleDef(module)(declarations =
      declarations, block = block)
  }
}

@component
trait TypeDefNamerComponent extends NamerComponent {
  (tdef: TypeDefApi) => {
    val tpt = name(tdef.tpt)
    tdef.symbol.foreach {
      case sym: TypeDefSymbol => sym.typeSymbol = tpt.symbol
      case _                  => ()
    }
    TreeCopiers.copyTypeDef(tdef)(tpt = tpt)
  }
}

@component
trait ClassDefNamerComponent extends NamerComponent {
  (clazz: ClassDefApi) => {
    val body    = name(clazz.body).asInstanceOf[TemplateApi]
    val fields  = body.members.flatMap {
      case v: ValDefApi     =>
        v.tpe.map((v.name, _))
      case _                => None
    }
    val tpe     = RecordType(fields.toMap)
    clazz.tpe = tpe
    clazz.symbol.foreach(_.tpe = Some(tpe))
    TreeCopiers.copyRecordDef(clazz)(body = body)
  }
}

@component
trait ArrayTypeUseNamerComponent extends NamerComponent {
  (tuse: ArrayTypeUseApi) => {
    val tpt  = name(tuse.tpt).asInstanceOf[UseTree]
    val size = name(tuse.size).asInstanceOf[Expr]
    tpt.tpe.map { tpe =>
      val tpe2 = ArrayType(tpe, size)
      tuse.tpe = tpe2
    }
    TreeCopiers.copyArrayTypeUse(tuse)(tpt = tpt, size = size)
  }
}

@component
trait BlockNamerComponent extends NamerComponent {
  (block: BlockApi) => {
    val stmts  = block.stmts.map(name(_))
    TreeCopiers.copyBlock(block)(stmts = stmts)
  }
}


@component
trait BinaryNamerComponent extends NamerComponent {
  (bin: BinaryApi)          => {
    val lhs = name(bin.lhs).asInstanceOf[Expr]
    val rhs = name(bin.rhs).asInstanceOf[Expr]
    TreeCopiers.copyBinary(bin)(lhs = lhs, rhs = rhs)
  }

}

@component
trait UnaryNamerComponent extends NamerComponent {
  (unary: UnaryApi)          => {
    val expr = name(unary.expr).asInstanceOf[Expr]
    TreeCopiers.copyUnary(unary)(expr = expr)
  }

}

@component
trait AssignNamerComponent extends NamerComponent {
  (assgn: AssignApi)          => {
    val lhs = name(assgn.lhs).asInstanceOf[Expr]
    val rhs = name(assgn.rhs).asInstanceOf[Expr]
    TreeCopiers.copyAssign(assgn)(lhs = lhs, rhs = rhs)
  }

}


@component
trait IfNamerComponent extends NamerComponent {
  (ifelse: IfApi)          => {
    val cond = name(ifelse.cond).asInstanceOf[Expr]
    val thenp = name(ifelse.thenp).asInstanceOf[Expr]
    val elsep = name(ifelse.elsep).asInstanceOf[Expr]
    TreeCopiers.copyIf(ifelse)(cond = cond, thenp = thenp,
      elsep = elsep)
  }

}

@component
trait WhileNamerComponent extends NamerComponent {
  (wile: WhileApi)          => {
    val cond = name(wile.cond).asInstanceOf[Expr]
    val body = name(wile.body).asInstanceOf[Expr]
    TreeCopiers.copyWhile(wile)(cond = cond, body = body)
  }

}

@component
trait ApplyNamerComponent extends NamerComponent {
  (apply: ApplyApi)          => {
    val fun = name(apply.fun).asInstanceOf[Expr]
    val args = apply.args.map { arg =>
      name(arg).asInstanceOf[Expr]
    }
    TreeCopiers.copyApply(apply)(fun = fun, args = args)
  }
}

@component
trait LiteralNamerComponent extends NamerComponent {
  (lit: LiteralApi)          => lit
}


@component
trait ArrayAccessNamerComponent extends NamerComponent {
  (access: ArrayAccessApi)          => {
    val array = name(access.array).asInstanceOf[Expr]
    val index = name(access.index).asInstanceOf[Expr]
    TreeCopiers.copyArrayAccess(access)(array = array, index = index)
  }
}


@component
trait SelectNamerComponent extends NamerComponent {
  (select: SelectApi) => {
    val qual           = name(select.qual)
    val slctdOwner     = qual.symbol match {
      case Some(vsym: VariableSymbol) => vsym.typeSymbol
      case s                          => s
    }
    println(slctdOwner.map(_.asInstanceOf[TypeDefSymbol].typeSymbol.map(_.declarations)))
    println(slctdOwner.map(_.getSymbol(Name("a"), _ => true)))
    slctdOwner.foreach(select.tree.owner = _)
    val tree           = name(select.tree).asInstanceOf[SimpleUseTree]
    tree.symbol.foreach(select.symbol = _)
    TreeCopiers.copySelect(select)(qual = qual, tree = tree)
  }
}
