package ch.usi.inf.l3.sana.ooj.typechecker


import ch.usi.inf.l3.sana
import sana.ooj
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj

import sana.dsl._
import tiny.ast.{TreeCopiers => _, TreeFactories => _, _}
import tiny.types.{TypeUtils => _, _}
import tiny.symbols.{TypeSymbol, TermSymbol, Symbol}
import tiny.source.Position
import tiny.names.Name
import tiny.errors.ErrorReporting.{error,warning}
import calcj.typechecker.{TyperComponent, TypePromotions}
import calcj.types._
import calcj.ast.operators.{Add, Eq, Neq, BOp}
import calcj.ast.BinaryApi
import primj.ast.{ApplyApi, ValDefApi}
import primj.symbols.{ProgramSymbol, MethodSymbol, VariableSymbol, ScopeSymbol}
import primj.types.{TypeUtils => _, _}
import ooj.modifiers.Ops._
import primj.modifiers.PARAM
import ooj.modifiers._
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
trait ProgramTyperComponent extends TyperComponent {
  (prg: ProgramApi) => {
    val members = prg.members.map(x => typed(x))
    TreeCopiers.copyProgram(prg)(members = members)
  }
}

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
    val members = pkg.members
      .map(member => typed(member).asInstanceOf[DefTree])
    TreeCopiers.copyPackageDef(pkg)(members = members)
  }
}

@component
trait ValDefTyperComponent extends TyperComponent {
  (valdef: ValDefApi)          => {
    if(!valdef.mods.isField) {
      checkDoubleDef(valdef.owner, valdef.name, valdef.pos)
      valdef.owner.foreach(sym => {
        valdef.symbol.foreach(sym.declare(_))
      })
    }
    val tpt    = typed(valdef.tpt).asInstanceOf[UseTree]
    valdef.symbol.foreach(sym => {
      sym.tpe.foreach(valdef.tpe = _)
      sym match {
        case vs: VariableSymbol =>
          vs.typeSymbol = tpt.symbol
        case _                  =>
          ()
      }
    })
    val rhs    = typed(valdef.rhs).asInstanceOf[Expr]
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
    } else (TypeUtils.isAssignable(rhs, rtpe, ttpe)) match {
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
      res.owner.map(sym => !(sym.isInstanceOf[ScopeSymbol] ||
            sym.isInstanceOf[MethodSymbol])).getOrElse(false)) {
      error(LOCAL_VARIABLE_OWNED_BY_NON_LOCAL,
        valdef.toString, "A local variable", valdef.pos)
    }


    res
  }

  protected def checkDoubleDef(owner: Option[Symbol],
      name: Name, pos: Option[Position]): Unit =
    if(SymbolUtils.alreadyDefinedLocalVarable(owner, name))
      error(VARIABLE_ALREADY_DEFINED,
          "", "", pos)
}

@component
trait ClassDefTyperComponent extends TyperComponent {
  (clazz: ClassDefApi) => {
    val parents =
      clazz.parents
        .map((parent) => typed(parent).asInstanceOf[UseTree])
    val body    = typed(clazz.body).asInstanceOf[TemplateApi]
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

    clazz.sourceName.foreach(sn => {
      if(clazz.mods.isPublicAcc && clazz.name.asString != sn)
        error(PUBLIC_CLASS_FILE_NAME_MATCH_ERROR,
          "", "", clazz.pos)
    })

    val methods       =
      body
        .members
        .filter(_.isInstanceOf[MethodDefApi])
        .map(_.asInstanceOf[MethodDefApi])


    reportDuplicateMethods(methods)


    if(clazz.mods.isFinal && (clazz.mods.isInterface || clazz.mods.isAbstract))
      error(ABSTRACT_FINAL,
          "", "", clazz.pos)

    TreeCopiers.copyClassDef(clazz)(body = body, parents = parents)
  }

  def reportDuplicateMethods(methodsToSee: List[MethodDefApi]): Unit = {
    methodsToSee match {
      case Nil                                 =>
        ()
      case (m::ms)                             =>
        val (matches, nonMatches) = ms.partition { mthd =>
          val mthdSym = mthd.symbol
          val msym    = m.symbol
          val mthdTpe = msym.flatMap(_.tpe)
          val mTpe    = mthdSym.flatMap(_.tpe)
          (mthdTpe, mTpe) match {
            case (Some(to: MethodType), Some(ts: MethodType)) =>
              msym.map(_.name) == mthdSym.map(_.name) &&
                to.params == ts.params &&
                mthdSym.map(_.mods.isConstructor) ==
                    msym.map(_.mods.isConstructor)
            case _                                            =>
              false
          }
        }
        matches foreach { x =>
          error(METHOD_ALREADY_DEFINED, "", "", x.pos)
        }
        reportDuplicateMethods(nonMatches)
    }
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

    parents.foreach { p =>
      if(p.symbol.map(s => s.mods.isFinal &&
          !s.mods.isAbstract && !s.mods.isInterface).getOrElse(false))
        error(FINAL_PARENT,
          "", "", p.pos)
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

@component
trait TemplateTyperComponent extends TyperComponent {
  (tmpl: TemplateApi) => {
    val members = tmpl.members.map { member =>
      typed(member)
    }
    TreeCopiers.copyTemplate(tmpl)(members = members)
  }
}


// TODO: Do we need this?
@component
trait MethodDefTyperComponent
  extends primj.typechecker.MethodDefTyperComponent {
  (mthd: MethodDefApi)          => {
    val params  = mthd.params.map { param =>
      typed(param).asInstanceOf[ValDefApi]
    }
    val body    = typed(mthd.body).asInstanceOf[Expr]
    val rtpe    = mthd.ret.tpe.getOrElse(ErrorType)
    val btpe    = body.tpe.getOrElse(ErrorType)
    val mods   = {
      (mthd.owner, mthd.symbol) match {
        case (Some(cs: ClassSymbol), Some(s)) if cs.overrides(s)=>
          s.mods = s.mods | OVERRIDE
          mthd.mods | OVERRIDE
        case _                                                     =>
          mthd.mods
      }
    }
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
      TreeCopiers.copyMethodDef(mthd)(mods = mods, body = body,
        params = params)
    }



    if(mods.isFinal && mods.isAbstract)
      error(ABSTRACT_FINAL,
          "", "", mthd.pos)

    mthd.owner match {
      case Some(cs: ClassSymbol)    =>
        val s = cs.getInheritedSymbol(mthd.name,
          s => {
            (s.tpe, mthd.tpe) match {
              case ((Some(ms: MethodType), Some(mt: MethodType))) =>
                ms.params == mt.params &&
                  s.mods.isFinal &&
                    mods.isOverride
              case _                                              =>
                false

            }
          })
        s.foreach( s => {
          if(mods.isOverride)
            error(OVERRIDING_FINAL_METHOD,
                "", "", mthd.pos)
        })
      case _                        =>
        ()
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


@component
trait ThisTyperComponent extends TyperComponent {
  (ths: ThisApi)                 => {
    val enclClass = ths.enclosingClassSymbol
    enclClass.foreach(ths.symbol = _)
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


@component
trait SuperTyperComponent extends TyperComponent {
  (spr: SuperApi)                 => {
    spr.enclosingClassSymbol match {
      case Some(csym: ClassSymbol)    =>
        csym.parents.filter(! _.mods.isInterface) match {
          case List(s)        =>
            spr.symbol = s
          case List(x, y)     =>
            if(x == SymbolUtils.objectClassSymbol) {
              spr.symbol = y
            } else if (y == SymbolUtils.objectClassSymbol) {
              spr.symbol = x
            }
          case _              =>
            ()
        }
      case _                          =>
        ()
    }
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


@component
trait NewTyperComponent extends TyperComponent {
  (nw: NewApi) => {
    val app     = typed(nw.app).asInstanceOf[ApplyApi]
    val tpe     = app match {
      case Apply(Select(qual, id), _) =>
        qual.symbol.foreach(nw.symbol = _)
        qual.tpe
      case _                          =>
        Some(ErrorType)
    }
    app match {
      case Apply(Select(qual, id), _) if id.name != StdNames.CONSTRUCTOR_NAME =>
        error(NAME_NOT_FOUND,
          id.name.asString, StdNames.CONSTRUCTOR_NAME.asString, id.pos)
      case _                                                                  =>
        ()
    }
    tpe.foreach(nw.tpe = _)
    TreeCopiers.copyNew(nw)(app = app)
  }
}


@component
trait ApplyTyperComponent extends TyperComponent {
  (apply: ApplyApi) => {
    val args   = apply.args.map(arg => typed(arg).asInstanceOf[Expr])
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
    val res = TreeCopiers.copyApply(apply)(fun = fun, args = args)
    if(isExplicitConstructorInvocation(res)) {
      args.foreach { arg =>
        if(isThisFieldAccess(arg)) {
          error(REFERENCE_FIELD_BEFORE_SUPERTYPE,
            "", "", arg.pos)
        }
      }
    }
    res
  }
  protected def isExplicitConstructorInvocation(tree: Tree): Boolean =
    TreeUtils.isExplicitConstructorInvocation(tree)

  protected def isThisFieldAccess(tree: Tree): Boolean =
    TreeUtils.isThisFieldAccess(tree)
}


@component
trait TypeUseTyperComponent
  extends primj.typechecker.TypeUseTyperComponent
  with TypeUseNamer {
  (tuse: TypeUseApi) => {
    val tuseCopy = if(!tuse.hasBeenNamed) {
      nameTypeUse(tuse)
    } else tuse
    super.apply(tuseCopy)
  }
}


trait TypeUseNamer {
  def nameTypeUse(tuse: TypeUseApi): TypeUseApi = {
    val tuseCopy = TreeCopiers.copyTypeUse(tuse)(name = tuse.name)
    tuseCopy.owner.foreach(sym => {
      sym.getSymbol(tuseCopy.name, _.isInstanceOf[TypeSymbol]) match {
        case Some(sym) => tuseCopy.symbol = sym
        case _         => ()
      }
    })
    tuseCopy.symbol match {
      case None          if tuseCopy.isQualified         =>
        // INFO:
        // In the class is private to the compilation unit, and
        // we are in the compilation unit then ignore the qualified
        // owner and search in the current compilation unit.
        for {
          opkg   <- SymbolUtils.enclosingPackage(tuseCopy.owner)
          epkg   <- SymbolUtils.enclosingPackage(tuseCopy.enclosing)
          encl   <-
            SymbolUtils.enclosingCompilationUnit(tuseCopy.enclosing)
              if opkg.defines(encl) &&  opkg == epkg
          sym    <- encl.getSymbol(tuseCopy.name, _.isInstanceOf[TypeSymbol])
          owner  <- sym.owner
        } {
          tuseCopy.symbol = sym
          tuseCopy.owner  = owner
        }
      case s                                             =>
        ()
    }
    tuseCopy
  }
}

@component
trait IdentTyperComponent extends primj.typechecker.IdentTyperComponent
      with ooj.namers.IdentNamer
      with ooj.typechecker.IdentNamer {
  (ident: IdentApi) => {
    if(!ident.hasBeenNamed) {
      val id = nameIdent(ident)
      id match {
        case id: IdentApi                          =>
          nameIdent(id, true)
        case tuse                                =>
          typed(tuse)
      }
    } else ident
  }


  // protected def alreadyDefined(sym: Symbol,
  //     encl: Option[Symbol], symbols: List[Symbol]): Boolean =
  //     if(sym.isInstanceOf[TermSymbol])
  //       if(sym.mods.isLocalVariable) {
  //         symbols.contains(sym)
  //       } else if(sym.mods.isField && isInStaticInit(encl)) {
  //         symbols.contains(sym)
  //       } else true
  //     else false

  // protected def isInStaticInit(symbol: Option[Symbol]): Boolean = {
  //   SymbolUtils.enclosingNonLocal(symbol)
  //     .map(_.mods.isStaticInit).getOrElse(false)
  // }
}


trait IdentNamer {
  def nameIdent(ident: IdentApi, isInTypeChecker: Boolean): IdentApi = {
    val id = TreeCopiers.copyIdent(ident)(name = ident.name)
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
                  applicableMethod(sym, tpes) && isAccessible(sym, from) &&
                      (sym.mods.isConstructor == id.isConstructorIdent)
                case _              =>
                  id.owner match {
                    case Some(from)     =>
                      applicableMethod(sym, tpes) &&
                          isAccessible(sym, from) &&
                            (sym.mods.isConstructor == id.isConstructorIdent)
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
            if(!mthd.mods.isStatic && id.shouldBeStatic &&
                !mthd.mods.isConstructor &&
                isInTypeChecker) {
              error(INSTANCE_METHOD_IN_STATIC_CONTEXT_INVOK,
                id.toString, "a method name", id.pos)
            } else {
              id.symbol = mthd
              id.symbol.flatMap(_.tpe).foreach(id.tpe    = _)
            }
          } else {
            enclosingNonLocal(id.owner).foreach { owner =>
              if(owner.mods.isStatic && !mthd.mods.isStatic &&
                  isInTypeChecker) {
                error(INSTANCE_METHOD_IN_STATIC_CONTEXT_INVOK,
                  id.toString, "a static method name", id.pos)
              } else {
                id.symbol = mthd
                id.symbol.flatMap(_.tpe).foreach(id.tpe    = _)
              }
            }
          }
        case (_::_)  if isInTypeChecker    =>
          error(AMBIGUOUS_METHOD_INVOCATION,
              id.toString, "a method name", id.pos)
        case Nil     if isInTypeChecker    =>
          error(NAME_NOT_FOUND,
              id.toString, "a method name", id.pos)
        case _                             =>
          ()
      }
      id
    } else if(id.isMethodIdent) {
      if(isInTypeChecker)
        error(INSTANTIATING_NON_CONCRETE_CLASS,
          id.toString, "a concrete class", id.pos)
      id
    } else {
      val symbol = id.owner.flatMap(
        _.getSymbol(id.name, _.isInstanceOf[TermSymbol]))

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

      symbol match {
        case Some(sym)                        =>
          enclosingNonLocal(id.owner).foreach { owner =>
            if(id.isQualified) {
              id.enclosing match {
                case Some(from) if !isAccessible(sym, from) &&
                                    isInTypeChecker           =>
                  error(FIELD_NOT_ACCESSIBLE,
                    id.toString, "an accessible name", id.pos)
                case _                                        =>
                  ()
              }
              if(sym.mods.isField && !sym.mods.isStatic &&
                  id.shouldBeStatic) {
                if(isInTypeChecker)
                  error(INSTANCE_FIELD_IN_STATIC_CONTEXT_INVOK,
                    id.toString, "a variable name", id.pos)
              } else {
                id.symbol = sym
                id.symbol.flatMap(_.tpe).foreach(id.tpe    = _)
              }
            } else if(isInStaticContext(id.owner) && sym.mods.isField &&
                  !sym.mods.isStatic) {
              if(isInTypeChecker)
                error(INSTANCE_FIELD_IN_STATIC_CONTEXT_INVOK,
                  id.toString, "a static name", id.pos)
            } else {
              if(!id.isQualified) {
                enclosingClass(id.owner) match {
                  case Some(encl: ClassSymbol) if
                                      !encl.directlyDefines(sym, _ => true) &&
                                             sym.mods.isPrivateAcc           =>
                    if(isInTypeChecker)
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
          if(isInTypeChecker)
            error(NAME_NOT_FOUND,
                id.toString, "a variable name", id.pos)
      }
      id
    }
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

@component
trait SelectTyperComponent extends TyperComponent {
  (select: SelectApi) => {
    val qual = typed(select.qual)
    qual.symbol.foreach(select.tree.owner = _)
    if(isType(qual)) {
      select.tree.shouldBeStatic = true
    }
    val tree = typed(select.tree).asInstanceOf[SimpleUseTree]
    tree.tpe.foreach(select.tpe = _)
    tree.symbol.foreach(select.symbol = _)
    TreeCopiers.copySelect(select)(qual, tree)
  }


  def isType(tree: Tree): Boolean = TreeUtils.isType(tree)
}



@component
trait BinaryTyperComponent extends calcj.typechecker.BinaryTyperComponent {

  override protected def binaryTyper(ltpe: Type,
    rtpe: Type, bin: BinaryApi): Option[(Type, Type, Type)] = bin.op match {
      case Eq | Neq    if rtpe.isInstanceOf[RefType]   &&
                          ltpe.isInstanceOf[RefType]             =>
        Some((ltpe, rtpe, BooleanType))
      case Add         if ltpe =:= TypeUtils.stringClassType ||
                          rtpe =:= TypeUtils.stringClassType     =>
        Some((TypeUtils.stringClassType, TypeUtils.stringClassType,
          TypeUtils.stringClassType))
      case _                                                     =>
        super.binaryTyper(ltpe, rtpe, bin)
    }



  // toString should be called when needed, also all primitives can be
  // easily promoted to String
  // in Java spec:
  // e is boolean               ==> new Boolean(e).toString();
  // e is char                  ==> new Character(e).toString();
  // e is byte, short or int    ==> new Integer(e).toString();
  // e is long                  ==> new Long(e).toString();
  // e is float                 ==> new Float(e).toString();
  // e is double                ==> new Double(e).toString();
  override protected def castIfNeeded(e: Expr, t1: Type, t2: Type): Expr = {
    val strTpe = TypeUtils.stringClassType
    val strSym = SymbolUtils.stringClassSymbol
    (t1, t2) match {
      case (`strTpe`, `strTpe`)                      =>
        e
      case (`strTpe`, t: PrimitiveType)              =>
        TypeUtils.toBoxedType(t) match {
          case Some(clazz: RefType)           =>
            // FIXME: This is all too ugly
            // We need to find a way to trigger the compiler from
            // the very first phase up to this phase, which helps
            // us avoid all these wirings
            val java  =
              TreeFactories.mkIdent(StdNames.JAVA_PACKAGE_NAME, e.pos)
            val lang  =
              TreeFactories.mkIdent(StdNames.LANG_PACKAGE_NAME, e.pos)
            val jl    = TreeFactories.mkSelect(java, lang, e.pos)
            val tuse  = TreeFactories.mkTypeUse(clazz.name, e.pos)
            val jlp   = TreeFactories.mkSelect(jl, tuse, e.pos)
            val cnstr = TreeFactories.mkIdent(StdNames.CONSTRUCTOR_NAME, e.pos)
            cnstr.isConstructorIdent = true
            val slct  = TreeFactories.mkSelect(jlp, cnstr, e.pos)
            val app   = TreeFactories.mkApply(slct, List(e), e.pos)
            val nw    = TreeFactories.mkNew(app)
            java.symbol = SymbolUtils.javaPackageSymbol
            lang.symbol = SymbolUtils.langPackageSymbol
            SymbolUtils.getSymbol(t).foreach { s =>
              SymbolUtils.toBoxedSymbol(s).foreach { s =>
                tuse.symbol = s
              }
            }
            nw.foreach(t => e.owner.foreach(t.owner = _))
            e.owner.foreach(s => {
              lang.enclosing = s
              tuse.enclosing = s
            })
            lang.isQualified = true
            tuse.isQualified = true
            java.owner = ProgramSymbol
            nw
          case _                              =>
            super.castIfNeeded(e, t1, t2)
        }
      case (`strTpe`, NullType)                      =>
        TreeFactories.mkLiteral(StringConstant("null"), e.pos)
      case (`strTpe`, r       )                      =>
        val slct = TreeFactories.mkSelect(e,
          TreeFactories.mkIdent(Name("toString"), e.pos), e.pos)
        TreeFactories.mkApply(slct, Nil, e.pos)
      case _                                         =>
        super.castIfNeeded(e, t1, t2)
    }
  }
}

@component
trait AssignTyperComponent extends primj.typechecker.AssignTyperComponent {
  override protected def checkFinalReassigning(lhs: Tree): Unit = {
    val isInStaticInit =
      SymbolUtils.enclosingNonLocal(lhs.owner)
        .map(_.mods.isStaticInit)
        .getOrElse(false)
    if(TreeUtils.isFinal(lhs) && !isInStaticInit)
      // INFO: Static init is final re-assigning is dealt with in control flow
      // checks
      error(REASSIGNING_FINAL_VARIABLE,
        lhs.toString, lhs.toString, lhs.pos)
  }
}
