package ch.usi.inf.l3.sana.ooj.typechecker


import ch.usi.inf.l3.sana
import sana.ooj
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj

import sana.dsl._
import tiny.ast.{TreeCopiers => _, _}
import tiny.types.{TypeUtils => _, _}
import tiny.symbols.{TypeSymbol, TermSymbol, Symbol}
import tiny.source.Position
import tiny.names.Name
import tiny.errors.ErrorReporting.{error,warning}
import calcj.typechecker.{TyperComponent, TypePromotions}
import calcj.types._
import primj.ast.{ApplyApi, ValDefApi}
import primj.symbols.{MethodSymbol, VariableSymbol}
import primj.types._
import ooj.modifiers.Ops._
import ooj.ast._
import ooj.names.StdNames
import ooj.symbols._
import ooj.types._
import ooj.ast.Implicits._
import ooj.errors.ErrorCodes._
import sana.ooj.ast.TreeExtractors._

/*
CompilationUnit: DONE
PackageDef: DONE
ClassDef: DONE
Template: DONE
MethodDef: DONE
New: DONE
Select: DONE
This: DONE
Super: DONE
TypeUse: DONE
Ident: DONE
Apply: DONE
*/

// Set class type information in a separate phase, then
// advance to typer

@component
trait CompilationUnitTyperComponent extends TyperComponent {
  (unit: CompilationUnitApi) => {
    val pkg = typed(unit.module).asInstanceOf[PackageDefApi]
    TreeCopiers.copyCompilationUnit(unit)(module = pkg)
  }
}

@component
trait PackageDefTyperComponent extends TyperComponent {
  (pkg: PackageDefApi) => {
    val members = pkg.members.map(member => typed(member).asInstanceOf[DefTree])
    TreeCopiers.copyPackageDef(pkg)(members = members)
  }
}

@component
trait ValDefTyperComponent extends primj.typechecker.ValDefTyperComponent {
  (valdef: ValDefApi)          => {
    val res = super.apply(valdef).asInstanceOf[ValDefApi]
    res.symbol.foreach(sym => {
      sym match {
        case vs: VariableSymbol =>
          vs.typeSymbol = res.tpt.symbol
        case _                  =>
          ()
      }
    })
    valdef.owner match {
      case Some(csym: ClassSymbol) if csym.mods.isInterface       =>
        if(!valdef.mods.isStatic)
          error(NON_STATIC_FIELD_IN_INTERFACE,
              valdef.toString, "A static final field", valdef.pos, valdef)
        if(!valdef.mods.isFinal)
          error(NON_FINAL_FIELD_IN_INTERFACE,
              valdef.toString, "A static final field", valdef.pos, valdef)
      case _                                                      =>
        ()
    }


    res
  }
}

@component
trait ClassDefTyperComponent extends TyperComponent {
  (clazz: ClassDefApi) => {
    val parents =
      clazz.parents.map((parent) => typed(parent).asInstanceOf[UseTree])
    val body    = typed(clazz.body).asInstanceOf[TemplateApi]
    TreeCopiers.copyClassDef(clazz)(body = body, parents = parents)
  }

  protected def packageName(symbol: ClassSymbol): String =
    SymbolUtils.packageName(symbol)
}

@component
trait TemplateTyperComponent extends TyperComponent {
  (tmpl: TemplateApi) => {
    val members = tmpl.members.map(typed(_))
    TreeCopiers.copyTemplate(tmpl)(members = members)
  }
}


// TODO: Do we need this?
@component
trait MethodDefTyperComponent
  extends primj.typechecker.MethodDefTyperComponent {
  (mthd: MethodDefApi)          => {
    val body    = typed(mthd.body).asInstanceOf[Expr]
    val rtpe    = mthd.ret.tpe.getOrElse(ErrorType)
    val btpe    = body.tpe.getOrElse(ErrorType)
    // if(!(btpe <:< rtpe) && rtpe =/= VoidType) {
    //   error(TYPE_MISMATCH,
    //       rtpe.toString, btpe.toString, body.pos, mthd)
    //   mthd
    // } else {
    // Check if all paths eventually return
    val res     = if(rtpe =/= VoidType && !allPathsReturn(body)) {
      error(MISSING_RETURN_STATEMENT,
        body.toString, body.toString, body.pos, mthd)
      mthd
    } else {
      TreeCopiers.copyMethodDef(mthd)(body = body)
    }

    if(mthd.mods.isAbstract && ! mthd.mods.isConstructor &&
        mthd.body != NoTree) {
      error(ABSTRACT_METHOD_CANNOT_HAVE_BODY,
          mthd.toString, "No constructor", mthd.pos, mthd)
    }

    if (mthd.mods.isAbstract && mthd.mods.isConstructor) {
      error(CONSTRUCTOR_CANNOT_BE_ABSTRACT,
          mthd.toString, "No constructor", mthd.pos, mthd)
    }


    mthd.owner match {
      case Some(sym) if mthd.mods.isInterface && mthd.mods.isConstructor =>
        error(CONSTRUCTOR_IN_INTERFACE,
            mthd.toString, "No constructor", mthd.pos, mthd)
      case Some(sym) if mthd.mods.isInterface =>
        error(NON_ABSTRACT_METHOD_IN_INTERFACE,
            mthd.toString, "An abstract method", mthd.pos, mthd)
      case Some(sym) if !(sym.mods.isInterface ||
                          sym.mods.isAbstract) &&
                          mthd.mods.isAbstract                           =>
        error(ABSTRACT_METHOD_IN_CONCRETE_CLASS,
            mthd.toString, "An abstract method", mthd.pos, mthd)
      case _                                                             =>
        ()
    }

    res

    // }
  }

  override def allPathsReturn(expr: Tree): Boolean = {
    enclosingMethod(expr.symbol) match {
      case Some(mthd)                         =>
        mthd.mods.isAbstract || TreeUtils.allPathsReturn(expr)
      case None                               =>
        expr == NoTree
    }
  }


  protected def enclosingMethod(symbol: Option[Symbol]): Option[Symbol] =
    SymbolUtils.enclosingMethod(symbol)

  protected def enclosingClass(symbol: Option[Symbol]): Option[Symbol] =
    SymbolUtils.enclosingClass(symbol)
}


@component
trait ThisTyperComponent extends TyperComponent {
  (ths: ThisApi)                 => {
    val enclClass = ths.enclosingClassSymbol
    val owner     = ths.owner
    enclClass match {
      case None                  =>
        error(ACCESSING_THIS_OUTSIDE_A_CLASS,
              ths.toString, "", ths.pos, ths)
      case _                     =>
        ()
    }

    val enclosing = SymbolUtils.enclosingNonLocal(owner)

    enclosing.foreach { sym => sym.mods.isStatic match {
        case true                  =>
          error(ACCESSING_THIS_IN_STATIC,
                ths.toString, "", ths.pos, ths)
        case false                 =>
          ()
      }
    }
    ths
  }
}


@component
trait SuperTyperComponent extends TyperComponent {
  (spr: SuperApi)                 => {
    val enclClass = spr.enclosingClassSymbol
    val owner     = spr.owner
    enclClass match {
      case None                                                     =>
        error(ACCESSING_SUPER_OUTSIDE_A_CLASS,
              spr.toString, "", spr.pos, spr)
      case Some(sym) if sym.tpe == Some(TypeUtils.objectClassType)  =>
        error(ACCESSING_SUPER_IN_OBJECT_CLASS,
              spr.toString, "", spr.pos, spr)
      case _                                                        =>
        ()
    }

    val enclosing = SymbolUtils.enclosingNonLocal(owner)

    enclosing.foreach { sym => sym.mods.isStatic match {
        case true                  =>
          error(ACCESSING_SUPER_IN_STATIC,
                spr.toString, "", spr.pos, spr)
        case false                 =>
          ()
      }
    }
    spr
  }
}


@component
trait NewTyperComponent extends TyperComponent {
  (nw: NewApi) => {
    val app     = typed(nw.app).asInstanceOf[ApplyApi]
    val tpe     = app match {
      case Apply(Select(qual, _), _) =>
        qual.symbol.foreach(nw.symbol = _)
        qual.tpe
      case _                      =>
        Some(ErrorType)
    }
    tpe.foreach(nw.tpe = _)
    TreeCopiers.copyNew(nw)(app = app)
  }
}


@component
trait ApplyTyperComponent extends TyperComponent {
  (apply: ApplyApi) => {
    val args   = apply.args.map(typed(_).asInstanceOf[Expr])
    val fun    = {
      apply.fun match {
        case fun@Select(qual, f: IdentApi)   =>
          f.isMethodIdent = true
          f.argumentTypes = args.flatMap(_.tpe)
          typed(fun).asInstanceOf[SelectApi]
        case f: IdentApi                     =>
          f.isMethodIdent = true
          f.argumentTypes = args.flatMap(_.tpe)
          typed(f).asInstanceOf[IdentApi]
      }
    }
    fun.tpe match {
      case Some(mtpe: MethodType) =>
        apply.tpe = mtpe.ret
      case _                      =>
        ()
    }
    fun.symbol match {
      case Some(m: MethodSymbol)   =>
        m.ret.foreach(apply.symbol = _)
      case _                       =>
        ()
    }
    TreeCopiers.copyApply(apply)(fun = fun, args = args)
  }
}


@component
trait TypeUseTyperComponent extends primj.typechecker.TypeUseTyperComponent {
  (tuse: TypeUseApi) => {
    tuse.owner.foreach(sym => {
      sym.getSymbol(tuse.name, _.isInstanceOf[TypeSymbol]) match {
        case Some(sym) => tuse.symbol = sym
        case _         => ()
      }
    })
    super.apply(tuse)
  }
}

@component
trait IdentTyperComponent extends primj.typechecker.IdentTyperComponent {
  (id: IdentApi) => {
    val tptMods = if(id.isQualified) {
      id.owner.map(_.mods).getOrElse(noflags)
    } else enclosingClass(id.owner).map(_.mods).getOrElse(noflags)

    if(id.isMethodIdent && !(id.name == StdNames.CONSTRUCTOR_NAME &&
              (tptMods.isAbstract || tptMods.isInterface))) {
      val allCandidateMethods = (enclosingClass(id.owner),
            id.argumentTypes) match {
        case (Some(cs: ClassSymbol), Some(tpes)) =>
          cs.getAllSymbols(id.name,
            (sym) => {
              id.enclosing match {
                case Some(from)     =>
                  applicableMethod(sym, tpes) && isAccessible(sym, from)
                case _              =>
                  id.owner match {
                    case Some(from)     =>
                      applicableMethod(sym, tpes) &&
                          isAccessible(sym, from)
                    case _              =>
                      false
                  }
              }
            }).toList
        case _                     =>
          Nil
      }
      val candidateMethods = mostSpecificMethods(allCandidateMethods)


      candidateMethods match {
        case List(mthd)                =>
          if(id.isQualified) {
            if(!mthd.mods.isStatic && id.shouldBeStatic) {
              error(INSTANCE_METHOD_IN_STATIC_CONTEXT_INVOK,
                id.toString, "a method name", id.pos, id)
            } else {
              id.symbol = mthd
              id.symbol.flatMap(_.tpe).foreach(id.tpe    = _)
            }
          } else {
            enclosingNonLocal(id.owner).foreach { owner =>
              if(owner.mods.isStatic && !mthd.mods.isStatic) {
                error(INSTANCE_METHOD_IN_STATIC_CONTEXT_INVOK,
                  id.toString, "a static method name", id.pos, id)
              } else {
                id.symbol = mthd
                id.symbol.flatMap(_.tpe).foreach(id.tpe    = _)
              }
            }
          }
        case (x::xs)                   =>
          error(AMBIGUOUS_METHOD_INVOCATION,
              id.toString, "a method name", id.pos, id)
        case Nil                       =>
          error(NAME_NOT_FOUND,
              id.toString, "a method name", id.pos, id)
      }
      id
    } else if(id.isMethodIdent) {
      error(INSTANTIATING_NON_CONCRETE_CLASS,
        id.toString, "a concrete class", id.pos, id)
      id
    } else {
      val symbol = id.owner.flatMap(_.getSymbol(id.name,
          _.isInstanceOf[TermSymbol]))
      symbol match {
        case Some(sym)                        =>
          enclosingNonLocal(id.owner).foreach { owner =>
            if(id.isQualified) {
              id.enclosing match {
                case Some(from) if !isAccessible(sym, from)   =>
                  error(FIELD_NOT_ACCESSIBLE,
                    id.toString, "an accessible name", id.pos, id)
                case _                                        =>
                  ()
              }
              if(sym.mods.isField && !sym.mods.isStatic && id.shouldBeStatic) {
                error(INSTANCE_FIELD_IN_STATIC_CONTEXT_INVOK,
                  id.toString, "a variable name", id.pos, id)
              } else {
                id.symbol = sym
                id.symbol.flatMap(_.tpe).foreach(id.tpe    = _)
              }
            } else if(owner.mods.isStatic && sym.mods.isField &&
                  !sym.mods.isStatic) {
              error(INSTANCE_FIELD_IN_STATIC_CONTEXT_INVOK,
                id.toString, "a static name", id.pos, id)
            } else {
              id.symbol = sym
              id.symbol.flatMap(_.tpe).foreach(id.tpe    = _)
            }
          }
        case _                                =>
          error(NAME_NOT_FOUND,
              id.toString, "a variable name", id.pos, id)
      }
      id

    }
  }


  protected def enclosingNonLocal(sym: Option[Symbol]): Option[Symbol] =
    SymbolUtils.enclosingNonLocal(sym)
  protected def enclosingClass(sym: Option[Symbol]): Option[Symbol] =
    SymbolUtils.enclosingClass(sym)

  protected def isAccessible(symbol: Symbol, from: Symbol): Boolean =
    SymbolUtils.isAccessible(symbol, from)

  protected def mostSpecificMethods(symbols: List[Symbol]): List[Symbol] =
    SymbolUtils.mostSpecificMethods(symbols)

  protected def applicableMethod(symbol: Symbol,
    atpes: List[Type]): Boolean = symbol match {
    case ms: MethodSymbol =>
      ms.tpe match {
        case Some(mt: MethodType)   =>
          SymbolUtils.methodCanBeApplied(mt.params, atpes)
        case _                      =>
          false
      }
    case _                =>
      false
  }
}



@component
trait SelectTyperComponent extends TyperComponent {
  (select: SelectApi) => {
    val qual = typed(select.qual)
    qual.symbol.foreach(select.tree.owner = _)
    val tree = typed(select.tree).asInstanceOf[SimpleUseTree]
    tree.tpe.foreach(select.tpe = _)
    tree.symbol.foreach(select.symbol = _)
    if(isType(qual)) {
      tree.shouldBeStatic = true
    }
    TreeCopiers.copySelect(select)(qual, tree)
  }


  def isType(tree: Tree): Boolean = TreeUtils.isType(tree)
}
