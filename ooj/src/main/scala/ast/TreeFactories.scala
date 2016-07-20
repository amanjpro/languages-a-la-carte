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
import sana.tiny.types.Type
import sana.tiny.source.Position
import sana.tiny.symbols.Symbol
import sana.tiny.names.Name
import sana.tiny.modifiers.Flags
import sana.tiny.ast._
import sana.calcj.ast._
import sana.calcj.ast.operators.{UOp, BOp}
import sana.primj.ast.{MethodDefApi => _, ProgramApi => _, _}
import sana.brokenj.ast.{TreeFactories => TF, _}
import sana.primj.types._
import sana.ooj.ast.Implicits._
import sana.ooj.ast.TreeExtractors._

trait TreeFactories {

  /**
   * Creates a new tree to represent ooj's program
   *
   * @param members the members of this program
   */
  def mkProgram(members: List[Tree]): ProgramApi = {
    new Program(members)
  }

  /**
   * Creates a new tree to represent a compilation unit
   *
   * @param module the package tree in this compilation unit
   * @param sourceName the name of the source file of this compilation unit
   * @param sourcePath the path of the source file of this compilation unit
   * @param symbol the symbol of this tree
   */
  def mkCompilationUnit(module: PackageDefApi, sourceName: String,
    sourcePath: List[String],
    symbol: Option[Symbol] = None): CompilationUnitApi = {
    val res = new CompilationUnit(module, sourceName, sourcePath)
    symbol.foreach(res.symbol = _)
    res
  }

  /**
   * Creates a new tree to represent a package definition in a source file.
   *
   * @param containingPackages the list of the packages that contain this package
   * @param name the name of this tree
   * @param members the list of the members of this package
   * @param pos the position of this tree
   * @param symbol the symbol of this tree
   */
  def mkPackageDef(containingPackages: List[Name],
    name: Name, members: List[Tree],
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None): PackageDefApi = {
    val res = new PackageDef(containingPackages, name, members)
    pos.foreach(res.pos = _)
    symbol.foreach(sym => {
      res.symbol = sym
      sym.owner.foreach(res.owner = _)
    })
    res
  }

  /**
   * Creates a new tree to represent a class definition
   *
   * @param mods the modifiers of this class
   * @param name the name of this class
   * @param parents the list of the parents of this class
   * @param body the body of this class
   * @param pos the position of this tree
   * @param symbol the symbol of this tree
   * @param tpe the type of this tree
   */
  def mkClassDef(mods: Flags, name: Name,
      parents: List[UseTree], body: TemplateApi,
      pos: Option[Position] = None,
      symbol: Option[Symbol] = None,
      tpe: Option[Type] = None): ClassDefApi = {
    val res = new ClassDef(mods, name, parents, body)
    pos.foreach(res.pos = _)
    symbol.foreach(sym => {
      res.symbol = sym
      sym.owner.foreach(res.owner = _)
    })
    symbol.flatMap(_.tpe).foreach(res.tpe = _)
    res
  }

  /**
   * Creates a new tree to represent the body of a class definition
   *
   * @param members the list of the members of this class-body
   * @param pos the position of this tree
   * @param owner the owner of this tree
   */
  def mkTemplate(members: List[Tree],
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): TemplateApi = {
    val res = new Template(members)
    pos.foreach(res.pos = _)
    owner.foreach(res.owner = _)
    res
  }

  /**
   * Creates an instance of a `new` expression
   *
   * @param app the method application part of this expression
   * @param pos the position of this tree
   * @param owner the owner of this tree
   */
  def mkNew(app: ApplyApi,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): NewApi = {
    val res = new New(app)
    pos.foreach(res.pos = _)
    owner.foreach(res.owner = _)
    app.fun match {
      case Apply(Select(qual, _), _) =>
        qual.tpe.foreach(res.tpe = _)
      case _                      =>
        ()
    }
    res
  }


  /**
   * Creates an instance of a member selection tree
   *
   * @param qual the tree that has been selected from
   * @param tree the tree that has been selected
   * @param pos the position of this tree
   * @param symbol the symbol of this tree
   * @param owner the owner of this tree
   */
  def mkSelect(qual: Tree, tree: SimpleUseTree,
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None,
    owner: Option[Symbol] = None): SelectApi = {
    val res = new Select(qual, tree)
    pos.foreach(res.pos = _)
    symbol.foreach(res.symbol = _)
    owner.foreach(res.owner = _)
    res
  }

  /**
   * Creates an instance of a `this` expression
   *
   * @param pos the position of this tree
   * @param enclosingClassSymbol the symbol of the class that enclosing `this`
   *                             expression
   * @param owner the owner of this tree
   */
  def mkThis(pos: Option[Position] = None,
      enclosingClassSymbol: Option[Symbol] = None,
      owner: Option[Symbol] = None): ThisApi = {
    val res = new This
    pos.foreach(res.pos = _)
    owner.foreach(res.owner = _)
    enclosingClassSymbol.foreach(sym => {
      res.enclosingClassSymbol = sym
      sym.tpe.foreach(res.tpe = _)
    })
    res
  }

  /**
   * Creates an instance of a `super` expression
   *
   * @param pos the position of this tree
   * @param enclosingClassSymbol the symbol of the class that enclosing this
   *                             `super` expression
   * @param owner the owner of this tree
   */
  def mkSuper(pos: Option[Position] = None,
      enclosingClassSymbol: Option[Symbol] = None,
      owner: Option[Symbol] = None): SuperApi = {
    val res = new Super
    pos.foreach(res.pos = _)
    owner.foreach(res.owner = _)
    enclosingClassSymbol.foreach(res.enclosingClassSymbol = _)

    // val tpe: TypeState[Type] = for {
    //   ctx       <- get
    //   ptpe      <- ctx.getTree(owner) match {
    //     case Some(t)          =>
    //       val ty = t.tpe.eval(ctx)
    //       ty match {
    //         case ct: ClassType     =>
    //           val pids = ct.parents.foldLeft(Nil: List[TreeId])((z, y) => {
    //             y match {
    //               case ct: ClassType       => (ct.id)::z
    //               case _                   => z
    //             }
    //           })
    //           pids.filter(ctx.isInterface(_)) match {
    //             case List(x)       => ctx.getTpe(x)
    //             case _             => None
    //           }
    //         case _                 => None
    //       }
    //     case None             => None
    //   }
    // } yield ptpe

    res
  }



  /** @see [[sana.brokenj.ast.TreeFactories.mkIdent]] */
  def mkIdent(name: Name,
            pos: Option[Position] = None,
            symbol: Option[Symbol] = None,
            enclosing: Option[Symbol]  = None,
            owner: Option[Symbol] = None): IdentApi = {
    val res = TF.mkIdent(name, pos, symbol, owner)
    enclosing.foreach(res.enclosing = _)
    res
  }

  /** @see [[sana.brokenj.ast.TreeFactories.mkTypeUse]] */
  def mkTypeUse(name: Name,
            pos: Option[Position] = None,
            symbol: Option[Symbol] = None,
            enclosing: Option[Symbol]  = None,
            owner: Option[Symbol] = None): TypeUseApi = {
    val res = TF.mkTypeUse(name, pos, symbol, owner)
    enclosing.foreach(res.enclosing = _)
    res
  }

  /**
   * Creates a new tree to represent a method definition
   *
   * @param mods the modifiers of this method
   * @param ret the type-tree of the return type of this method
   * @param name the name of this method
   * @param params the list of the parameters of this method
   * @param body the body of this method
   * @param pos the position of this tree
   * @param symbol the symbol of this tree
   * @param tpe the type of this tree
   */
  def mkMethodDef(mods: Flags, ret: UseTree,
    name: Name, params: List[ValDefApi],
    body: Expr, pos: Option[Position] = None,
    symbol: Option[Symbol] = None): MethodDefApi = {
    val res = new MethodDef(mods, ret, name, params, body)
    pos.foreach(res.pos = _)
    symbol.foreach( sym => {
      res.symbol = sym
      sym.owner.foreach(res.owner = _)
    })
    val tys = params.flatMap(_.tpe)
    ret.tpe.foreach(t => res.tpe = MethodType(t, tys))
    res
  }


  // Delegating already working ones to brokenj.ast.TreeFactories

  // From calcj
  /** @see [[sana.brokenj.ast.TreeFactories.mkCast]] */
  def mkCast(tpt: UseTree, expr: Expr,
           pos: Option[Position] = None): CastApi = {
    TF.mkCast(tpt, expr, pos)
  }


  /** @see [[sana.brokenj.ast.TreeFactories.mkLiteral]] */
  def mkLiteral(constant: Constant,
              pos: Option[Position] = None,
              owner: Option[Symbol] = None): LiteralApi = {
    TF.mkLiteral(constant, pos, owner)
  }


  /** @see [[sana.brokenj.ast.TreeFactories.mkBinary]] */
  def mkBinary(lhs: Expr, op: BOp, rhs: Expr,
              pos: Option[Position] = None,
              tpe: Option[Type]     = None,
              owner: Option[Symbol] = None): BinaryApi = {
    TF.mkBinary(lhs, op, rhs, pos, tpe, owner)
  }

  /** @see [[sana.brokenj.ast.TreeFactories.mkUnary]] */
  def mkUnary(isPostfix: Boolean, op: UOp, expr: Expr,
              pos: Option[Position] = None,
              tpe: Option[Type]     = None,
              owner: Option[Symbol] = None): UnaryApi = {
    TF.mkUnary(isPostfix, op, expr, pos, tpe, owner)
  }

  // From primj


  /** @see [[sana.brokenj.ast.TreeFactories.mkAssign]] */
  def mkAssign(lhs: Expr, rhs: Expr,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): AssignApi = {
    TF.mkAssign(lhs, rhs, pos, owner)
  }


  /** @see [[sana.brokenj.ast.TreeFactories.mkIf]] */
  def mkIf(cond: Expr, thenp: Expr, elsep: Expr,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): IfApi = {
    TF.mkIf(cond, thenp, elsep, pos, owner)
  }


  /** @see [[sana.brokenj.ast.TreeFactories.mkWhile]] */
  def mkWhile(isDoWhile: Boolean, cond: Expr, body: Expr,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): WhileApi = {
    TF.mkWhile(isDoWhile, cond, body, pos, owner)
  }

  /** @see [[sana.brokenj.ast.TreeFactories.mkFor]] */
  def mkFor(inits: List[Tree], cond: Expr, steps: List[Expr],
    body: Expr, pos: Option[Position] = None,
    symbol: Option[Symbol] = None): ForApi = {
    TF.mkFor(inits, cond, steps, body, pos, symbol)
  }

  /** @see [[sana.brokenj.ast.TreeFactories.mkBlock]] */
  def mkBlock(stmts: List[Tree],
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None): BlockApi = {
    TF.mkBlock(stmts, pos, symbol)
  }

  /** @see [[sana.brokenj.ast.TreeFactories.mkTernary]] */
  def mkTernary(cond: Expr, thenp: Expr, elsep: Expr,
    pos: Option[Position] = None,
    tpe: Option[Type]     = None,
    owner: Option[Symbol] = None): TernaryApi = {
    TF.mkTernary(cond, thenp, elsep, pos, tpe, owner)
  }


  /** @see [[sana.brokenj.ast.TreeFactories.mkApply]] */
  def mkApply(fun: Expr, args: List[Expr],
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): ApplyApi = {
    TF.mkApply(fun, args, pos, owner)
  }

  /** @see [[sana.brokenj.ast.TreeFactories.mkReturn]] */
  def mkReturn(expr: Option[Expr],
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): ReturnApi = {
    TF.mkReturn(expr, pos, owner)
  }



  /** @see [[sana.brokenj.ast.TreeFactories.mkValDef]] */
  def mkValDef(mods: Flags, tpt: UseTree, name: Name,
    rhs: Expr, pos: Option[Position] = None,
    symbol: Option[Symbol] = None): ValDefApi = {

    TF.mkValDef(mods, tpt, name, rhs, pos, symbol)
  }

  // brokenj
  /** @see [[sana.brokenj.ast.TreeFactories.mkLabel]] */
  def mkLabel(name: Name, stmt: Expr,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): LabelApi = {
    TF.mkLabel(name, stmt, pos, owner)
  }

  /** @see [[sana.brokenj.ast.TreeFactories.mkBreak]] */
  def mkBreak(label: Option[Name],
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): BreakApi = {
    TF.mkBreak(label, pos, owner)
  }

  /** @see [[sana.brokenj.ast.TreeFactories.mkContinue]] */
  def mkContinue(label: Option[Name],
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): ContinueApi = {
    TF.mkContinue(label, pos, owner)
  }

  /** @see [[sana.brokenj.ast.TreeFactories.mkCase]] */
  def mkCase(guards: List[Expr], body: Tree,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): CaseApi = {
    TF.mkCase(guards, body, pos, owner)
  }

  /** @see [[sana.brokenj.ast.TreeFactories.mkSwitch]] */
  def mkSwitch(expr: Expr, cases: List[CaseApi],
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): SwitchApi = {
    TF.mkSwitch(expr, cases, pos, owner)
  }
}


object TreeFactories extends TreeFactories
