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
import primj.symbols.{MethodSymbol, VariableSymbol, ScopeSymbol}
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

@component(tree, symbols)
trait CompilationUnitTyperComponent extends TyperComponent {
  (unit: CompilationUnitApi) => {
    val pkg = typed((unit.module, symbols)).asInstanceOf[PackageDefApi]
    TreeCopiers.copyCompilationUnit(unit)(module = pkg)
  }
}

@component(tree, symbols)
trait PackageDefTyperComponent extends TyperComponent {
  (pkg: PackageDefApi) => {
    val members = pkg.members
      .map(member => typed((member, symbols)).asInstanceOf[DefTree])
    TreeCopiers.copyPackageDef(pkg)(members = members)
  }
}

@component(tree, symbols)
trait ValDefTyperComponent extends TyperComponent {
  (valdef: ValDefApi)          => {
    val tpt    = typed((valdef.tpt, symbols)).asInstanceOf[UseTree]
    val rhs    = typed((valdef.rhs, symbols)).asInstanceOf[Expr]
    val rtpe   = rhs.tpe.getOrElse(ErrorType)
    val ttpe   = tpt.tpe.getOrElse(ErrorType)
    valdef.tpe = ttpe
    val res = if(ttpe =:= VoidType) {
      error(VOID_VARIABLE_TYPE,
          ttpe.toString, ttpe.toString, rhs.pos)
      valdef
    // } else if(valdef.mods.isFinal && !valdef.mods.isParam &&
    //           rhs == NoTree) {
    //   error(UNINITIALIZED_FINAL_VARIABLE,
    //       valdef.toString, "", valdef.pos, valdef)
    //   valdef
    } else (rtpe <:< ttpe) match {
        case false if rhs != NoTree        =>
          error(TYPE_MISMATCH,
            rtpe.toString, ttpe.toString, rhs.pos)
          valdef
        case _                             =>
          TreeCopiers.copyValDef(valdef)(tpt = tpt, rhs = rhs)
      }

    res.symbol.foreach(sym => {
      sym match {
        case vs: VariableSymbol =>
          vs.typeSymbol = res.tpt.symbol
        case _                  =>
          ()
      }
    })

    res.owner match {
      case Some(csym: ClassSymbol) if csym.mods.isInterface       =>
        if(!res.mods.isStatic)
          error(NON_STATIC_FIELD_IN_INTERFACE,
              valdef.toString, "A static final field", valdef.pos)
        if(!res.mods.isFinal)
          error(NON_FINAL_FIELD_IN_INTERFACE,
              valdef.toString, "A static final field", valdef.pos)
      case _                                                      =>
        ()
    }


    if(res.mods.isField &&
      res.owner.map(! _.isInstanceOf[TypeSymbol]).getOrElse(false)) {
      error(FIELD_OWNED_BY_NON_CLASS,
        valdef.toString, "A field", valdef.pos)
    } else if(res.mods.isParam &&
      res.owner.map(! _.isInstanceOf[MethodSymbol]).getOrElse(false)) {
      error(PARAM_OWNED_BY_NON_METHOD,
        valdef.toString, "A parameter", valdef.pos)
    } else if(res.mods.isLocalVariable &&
      res.owner.map(! _.isInstanceOf[ScopeSymbol]).getOrElse(false)) {
      error(LOCAL_VARIABLE_OWNED_BY_NON_LOCAL,
        valdef.toString, "A local variable", valdef.pos)
    }


    res
  }
}

@component(tree, symbols)
trait ClassDefTyperComponent extends TyperComponent {
  (clazz: ClassDefApi) => {
    val parents =
      clazz.parents
        .map((parent) => typed((parent, symbols)).asInstanceOf[UseTree])
    val body    = typed((clazz.body, symbols)).asInstanceOf[TemplateApi]
    if(!(clazz.mods.isInterface || clazz.mods.isAbstract)) {
      SymbolUtils.allAbstractMembers(clazz.symbol) match {
        case Nil                           =>
          ()
        case members                       =>
          error(NON_IMPLEMENTED_METHODS,
            clazz.name.asString, members.map(_.name).mkString(", "),
            clazz.pos)
      }
    }


    checkParents(parents, clazz)

    TreeCopiers.copyClassDef(clazz)(body = body, parents = parents)
  }



  protected def checkParents(parents: List[UseTree],
      clazz: ClassDefApi): Unit = {
    parents.foreach( p => {
      if(! (isInExtendsClause(p) || isInImplementsClause(p)))
        p match {
          case tuse: TypeUseApi            =>
            tuse.isInExtendsClause = true
          case Select(_, tuse: TypeUseApi) =>
            tuse.isInExtendsClause = true
          case _                           =>
            ()
        }
    })
    parents.filter(isInExtendsClause(_)) match {
      case List(x) if !clazz.mods.isInterface   =>
        isInterface(x.symbol) match {
          case true                    =>
            error(EXTENDING_AN_INTERFACE,
              x.name.asString, "A class type",
              x.pos)
          case _                       =>
            // pass
            ()
        }
      case List(x) if isObject(x.symbol)        =>
        // pass
        ()
      case List(x, y) if !clazz.mods.isInterface =>
        if(isObject(x.symbol) && ! isInterface(y.symbol)) {
          // pass
          ()
        } else if(isObject(y.symbol) && ! isInterface(x.symbol)) {
          // pass
          ()
        } else if (isObject(x.symbol)) {
          error(EXTENDING_AN_INTERFACE,
            x.name.asString, "A class type",
            x.pos)
        } else if (isObject(y.symbol)) {
          error(EXTENDING_AN_INTERFACE,
            y.name.asString, "A class type",
            y.pos)
        } else {
          error(CLASS_SHOULD_EXTEND_EXACTlY_ONE_CLASS,
            y.name.asString, "A class type",
            y.pos)
        }
      case Nil  if !clazz.mods.isInterface &&
                !isObject(clazz.symbol)          =>
        error(CLASS_SHOULD_EXTEND_EXACTlY_ONE_CLASS,
          clazz.name.asString, "A class type",
          clazz.pos)
      case _    if !clazz.mods.isInterface       =>
        error(CLASS_SHOULD_EXTEND_EXACTlY_ONE_CLASS,
          clazz.name.asString, "A class type",
          clazz.pos)
      case Nil                                   =>
        // pass
        ()
      case _                                     =>
        error(CLASS_SHOULD_EXTEND_EXACTlY_ONE_CLASS,
          clazz.name.asString, "A class type",
          clazz.pos)
    }

    parents.filter(isInImplementsClause(_)).foreach { tuse =>
      tuse.symbol match {
        case Some(sym) if !sym.mods.isInterface =>
          error(IMPLEMENTING_A_CLASS,
            sym.name.asString, "An interface type",
            tuse.pos)
        case _                                  =>
          // pass
          ()
      }
    }
  }


  protected def isInterface(symbol: Option[Symbol]): Boolean =
    symbol.map(_.mods.isInterface).getOrElse(false)

  protected def isObject(symbol: Option[Symbol]): Boolean =
    symbol.map(_ == SymbolUtils.objectClassSymbol).getOrElse(false)

  protected def packageName(symbol: ClassSymbol): String =
    SymbolUtils.packageName(symbol)


  protected def isInImplementsClause(tree: UseTree): Boolean =
    TreeUtils.isInImplementsClause(tree)

  protected def isInExtendsClause(tree: UseTree): Boolean =
    TreeUtils.isInExtendsClause(tree)
}

@component(tree, symbols)
trait TemplateTyperComponent extends TyperComponent {
  (tmpl: TemplateApi) => {
    val (members, _) = {
      tmpl.members.foldLeft((Nil: List[Tree], symbols))((z, member) => {
        val members  = z._1
        val symbols  = z._2

        val newSymbols: List[Symbol] = member match {
          case v: ValDefApi =>
            (v.symbol).map(_::symbols).getOrElse(symbols)
          case _            =>
            symbols
        }
        val r = typed((member, newSymbols))
        (members ++ List(r), newSymbols)
      })
    }
    TreeCopiers.copyTemplate(tmpl)(members = members)
  }
}


// TODO: Do we need this?
@component(tree, symbols)
trait MethodDefTyperComponent
  extends primj.typechecker.MethodDefTyperComponent {
  (mthd: MethodDefApi)          => {
    val body    = typed((mthd.body, symbols)).asInstanceOf[Expr]
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
        body.toString, body.toString, body.pos)
      mthd
    } else {
      TreeCopiers.copyMethodDef(mthd)(body = body)
    }

    if(mthd.mods.isAbstract && ! isConstructor(mthd.symbol) &&
        mthd.body != NoTree) {
      error(ABSTRACT_METHOD_CANNOT_HAVE_BODY,
          mthd.toString, "No body", mthd.pos)
    }

    if (mthd.mods.isAbstract && isConstructor(mthd.symbol)) {
      error(CONSTRUCTOR_CANNOT_BE_ABSTRACT,
          mthd.toString, "A concrete constructor", mthd.pos)
    }


    mthd.declaredClassNameForConstructor.foreach( nme => {
      val cnme = enclosingClass(mthd.symbol).map(_.name)
      if(cnme != Some(nme)) {
        error(CONSTRUCTOR_SHOULD_HAVE_THE_SAME_TYPE_AS_CONTAINING_CLASS,
          nme.asString,
          cnme.map(_.asString).getOrElse(StdNames.noname.asString),
          mthd.ret.pos)
      }
    })


    mthd.owner match {
      case Some(sym) if mthd.mods.isInterface &&
                        isConstructor(mthd.symbol)                       =>
        error(CONSTRUCTOR_IN_INTERFACE,
            mthd.toString, "No constructor", mthd.pos)
      case Some(sym) if mthd.mods.isInterface =>
        error(NON_ABSTRACT_METHOD_IN_INTERFACE,
            mthd.toString, "An abstract method", mthd.pos)
      case Some(sym) if !(sym.mods.isInterface ||
                          sym.mods.isAbstract) &&
                          mthd.mods.isAbstract                           =>
        error(ABSTRACT_METHOD_IN_CONCRETE_CLASS,
            mthd.toString, "An abstract method", mthd.pos)
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

  protected def isConstructor(symbol: Option[Symbol]): Boolean =
    symbol.map(SymbolUtils.isConstructor(_)).getOrElse(false)

  protected def enclosingMethod(symbol: Option[Symbol]): Option[Symbol] =
    SymbolUtils.enclosingMethod(symbol)

  protected def enclosingClass(symbol: Option[Symbol]): Option[Symbol] =
    SymbolUtils.enclosingClass(symbol)
}


@component(tree, symbols)
trait ThisTyperComponent extends TyperComponent {
  (ths: ThisApi)                 => {
    val enclClass = ths.enclosingClassSymbol
    val owner     = ths.owner
    enclClass match {
      case None                  =>
        error(ACCESSING_THIS_OUTSIDE_A_CLASS,
              ths.toString, "", ths.pos)
      case _                     =>
        ()
    }

    val enclosing = SymbolUtils.enclosingNonLocal(owner)

    enclosing.foreach { sym => sym.mods.isStatic match {
        case true                  =>
          error(ACCESSING_THIS_IN_STATIC,
                ths.toString, "", ths.pos)
        case false                 =>
          ()
      }
    }
    ths
  }
}


@component(tree, symbols)
trait SuperTyperComponent extends TyperComponent {
  (spr: SuperApi)                 => {
    val enclClass = spr.enclosingClassSymbol
    val owner     = spr.owner
    enclClass match {
      case None                                                     =>
        error(ACCESSING_SUPER_OUTSIDE_A_CLASS,
              spr.toString, "", spr.pos)
      case Some(sym) if sym.tpe == Some(TypeUtils.objectClassType)  =>
        error(ACCESSING_SUPER_IN_OBJECT_CLASS,
              spr.toString, "", spr.pos)
      case _                                                        =>
        ()
    }

    val enclosing = SymbolUtils.enclosingNonLocal(owner)

    enclosing.foreach { sym => sym.mods.isStatic match {
        case true                  =>
          error(ACCESSING_SUPER_IN_STATIC,
                spr.toString, "", spr.pos)
        case false                 =>
          ()
      }
    }
    spr
  }
}


@component(tree, symbols)
trait NewTyperComponent extends TyperComponent {
  (nw: NewApi) => {
    val app     = typed((nw.app, symbols)).asInstanceOf[ApplyApi]
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


@component(tree, symbols)
trait ApplyTyperComponent extends TyperComponent {
  (apply: ApplyApi) => {
    val args   = apply.args.map(arg => typed((arg, symbols)).asInstanceOf[Expr])
    val fun    = {
      apply.fun match {
        case fun@Select(qual, f: IdentApi)   =>
          f.isMethodIdent = true
          f.argumentTypes = args.flatMap(_.tpe)
          typed((fun, symbols)).asInstanceOf[SelectApi]
        case f: IdentApi                     =>
          f.isMethodIdent = true
          f.argumentTypes = args.flatMap(_.tpe)
          typed((f, symbols)).asInstanceOf[IdentApi]
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


@component(tree, symbols)
trait TypeUseTyperComponent extends primj.typechecker.TypeUseTyperComponent {
  (tuse: TypeUseApi) => {
    tuse.owner.foreach(sym => {
      sym.getSymbol(tuse.name, _.isInstanceOf[TypeSymbol]) match {
        case Some(sym) => tuse.symbol = sym
        case _         => ()
      }
    })
    super.apply((tuse, symbols))
  }
}

@component(tree, symbols)
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
                id.toString, "a method name", id.pos)
            } else {
              id.symbol = mthd
              id.symbol.flatMap(_.tpe).foreach(id.tpe    = _)
            }
          } else {
            enclosingNonLocal(id.owner).foreach { owner =>
              if(owner.mods.isStatic && !mthd.mods.isStatic) {
                error(INSTANCE_METHOD_IN_STATIC_CONTEXT_INVOK,
                  id.toString, "a static method name", id.pos)
              } else {
                id.symbol = mthd
                id.symbol.flatMap(_.tpe).foreach(id.tpe    = _)
              }
            }
          }
        case (_::_)                    =>
          error(AMBIGUOUS_METHOD_INVOCATION,
              id.toString, "a method name", id.pos)
        case Nil                       =>
          error(NAME_NOT_FOUND,
              id.toString, "a method name", id.pos)
      }
      id
    } else if(id.isMethodIdent) {
      error(INSTANTIATING_NON_CONCRETE_CLASS,
        id.toString, "a concrete class", id.pos)
      id
    } else {
      val symbol = id.owner.flatMap(
        _.getSymbol(id.name, sym => alreadyDefined(sym, id.owner, symbols)))

      //       s => {
      //         enclosingClass(id.owner) match {
      //           case Some(enc: ClassSymbol) =>
      //             s.isInstanceOf[TermSymbol] &&
      //             ((s.mods.isPrivateAcc && enc.directlyDefines(s)) ||
      //               !s.mods.isPrivateAcc)
      //           case _                      =>
      //             s.isInstanceOf[TermSymbol]
      //         }
      //       })
      // }

      symbol match {
        case Some(sym)                        =>
          enclosingNonLocal(id.owner).foreach { owner =>
            if(id.isQualified) {
              id.enclosing match {
                case Some(from) if !isAccessible(sym, from)   =>
                  error(FIELD_NOT_ACCESSIBLE,
                    id.toString, "an accessible name", id.pos)
                case _                                        =>
                  ()
              }
              if(sym.mods.isField && !sym.mods.isStatic && id.shouldBeStatic) {
                error(INSTANCE_FIELD_IN_STATIC_CONTEXT_INVOK,
                  id.toString, "a variable name", id.pos)
              } else {
                id.symbol = sym
                id.symbol.flatMap(_.tpe).foreach(id.tpe    = _)
              }
            } else if(isInStaticContext(id.owner) && sym.mods.isField &&
                  !sym.mods.isStatic) {
              error(INSTANCE_FIELD_IN_STATIC_CONTEXT_INVOK,
                id.toString, "a static name", id.pos)
            } else {
              if(!id.isQualified) {
                enclosingClass(id.owner) match {
                  case Some(encl: ClassSymbol) if !encl.directlyDefines(sym) &&
                                             sym.mods.isPrivateAcc           =>
                      error(FIELD_NOT_ACCESSIBLE,
                        id.toString, "an accessible name", id.pos)
                  case _                                                     =>
                    id.symbol = sym
                    id.symbol.flatMap(_.tpe).foreach(id.tpe    = _)
                }
              } else {
                id.symbol = sym
                id.symbol.flatMap(_.tpe).foreach(id.tpe    = _)
              }
            }
          }
        case _                                =>
          error(NAME_NOT_FOUND,
              id.toString, "a variable name", id.pos)
      }
      id

    }
  }


  protected def alreadyDefined(sym: Symbol,
      encl: Option[Symbol], symbols: List[Symbol]): Boolean =
      if(sym.isInstanceOf[TermSymbol])
        if(sym.mods.isLocalVariable) {
          symbols.contains(sym)
        } else if(sym.mods.isField && isInStaticInit(encl)) {
          symbols.contains(sym)
        } else true
      else false

  protected def isInStaticInit(symbol: Option[Symbol]): Boolean = {
    SymbolUtils.enclosingNonLocal(symbol)
      .map(_.mods.isStaticInit).getOrElse(false)
  }

  protected def isInStaticContext(symbol: Option[Symbol]): Boolean = {
    SymbolUtils.enclosingNonLocal(symbol)
      .map(_.mods.isStatic).getOrElse(false)
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



@component(tree, symbols)
trait SelectTyperComponent extends TyperComponent {
  (select: SelectApi) => {
    val qual = typed((select.qual, symbols))
    qual.symbol.foreach(select.tree.owner = _)
    val tree = typed((select.tree, symbols)).asInstanceOf[SimpleUseTree]
    tree.tpe.foreach(select.tpe = _)
    tree.symbol.foreach(select.symbol = _)
    if(isType(qual)) {
      tree.shouldBeStatic = true
    }
    TreeCopiers.copySelect(select)(qual, tree)
  }


  def isType(tree: Tree): Boolean = TreeUtils.isType(tree)
}
