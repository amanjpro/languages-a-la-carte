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
import sana.primj.ast.{MethodDef => _, _}
import sana.brokenj.ast.{TreeFactories => TF, _}
import sana.primj.types._
import sana.ooj.ast.Implicits._

trait TreeFactories {

  def mkPackageDef(mods: Flags, name: Name, members: List[Tree],
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None): PackageDefApi = {
    val res = PackageDef(mods, name, members)
    pos.foreach(res.pos = _)
    symbol.foreach(sym => {
      res.symbol = sym
      sym.owner.foreach(res.owner = _)
    })
    res
  }

  def mkClassDef(mods: Flags, name: Name,
      parents: List[UseTree], body: TemplateApi,
      pos: Option[Position] = None,
      symbol: Option[Symbol] = None,
      tpe: Option[Type] = None): ClassDefApi = {
    val res = ClassDef(mods, name, parents, body)
    pos.foreach(res.pos = _)
    symbol.foreach(sym => {
      res.symbol = sym
      sym.owner.foreach(res.owner = _)
    })
    symbol.flatMap(_.tpe).foreach(res.tpe = _)
    res
  }


  def mkTemplate(members: List[Tree],
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): TemplateApi = {
    val res = Template(members)
    pos.foreach(res.pos = _)
    owner.foreach(res.owner = _)
    res
  }


  def mkNew(tpt: UseTree,
    args: List[Expr],
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): NewApi = {
    val res = New(tpt, args)
    pos.foreach(res.pos = _)
    owner.foreach(res.owner = _)
    tpt.tpe.foreach(res.tpe = _)
    res
  }


  def mkSelect(qual: Tree, tree: SimpleUseTree,
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None,
    owner: Option[Symbol] = None): SelectApi = {
    val res = Select(qual, tree)
    pos.foreach(res.pos = _)
    symbol.foreach(res.symbol = _)
    owner.foreach(res.owner = _)
    res
  }

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



  def mkIdent(name: Name,
            pos: Option[Position] = None,
            symbol: Option[Symbol] = None,
            enclosing: Option[Symbol]  = None,
            owner: Option[Symbol] = None): IdentApi = {
    val res = TF.mkIdent(name, pos, symbol, owner)
    enclosing.foreach(res.enclosing = _)
    res
  }

  def mkTypeUse(name: Name,
            pos: Option[Position] = None,
            symbol: Option[Symbol] = None,
            enclosing: Option[Symbol]  = None,
            owner: Option[Symbol] = None): TypeUseApi = {
    val res = TF.mkTypeUse(name, pos, symbol, owner)
    enclosing.foreach(res.enclosing = _)
    res
  }

  def mkMethodDef(mods: Flags, ret: UseTree,
    name: Name, params: List[ValDefApi],
    body: Expr, pos: Option[Position] = None,
    symbol: Option[Symbol] = None): MethodDefApi = {
    val res = MethodDef(mods, ret, name, params, body)
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
  def mkCast(tpt: UseTree, expr: Expr,
           pos: Option[Position] = None): CastApi = {
    TF.mkCast(tpt, expr, pos)
  }


  def mkLiteral(constant: Constant,
              pos: Option[Position] = None,
              owner: Option[Symbol] = None): LiteralApi = {
    TF.mkLiteral(constant, pos, owner)
  }


  def mkBinary(lhs: Expr, op: BOp, rhs: Expr,
              pos: Option[Position] = None,
              tpe: Option[Type]     = None,
              owner: Option[Symbol] = None): BinaryApi = {
    TF.mkBinary(lhs, op, rhs, pos, tpe, owner)
  }

  def mkUnary(isPostfix: Boolean, op: UOp, expr: Expr,
              pos: Option[Position] = None,
              tpe: Option[Type]     = None,
              owner: Option[Symbol] = None): UnaryApi = {
    TF.mkUnary(isPostfix, op, expr, pos, tpe, owner)
  }

  // From primj
  def mkAssign(lhs: Expr, rhs: Expr,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): AssignApi = {
    TF.mkAssign(lhs, rhs, pos, owner)
  }


  def mkIf(cond: Expr, thenp: Expr, elsep: Expr,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): IfApi = {
    TF.mkIf(cond, thenp, elsep, pos, owner)
  }


  def mkWhile(isDoWhile: Boolean, cond: Expr, body: Expr,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): WhileApi = {
    TF.mkWhile(isDoWhile, cond, body, pos, owner)
  }

  def mkFor(inits: List[Tree], cond: Expr, steps: List[Expr],
    body: Expr, pos: Option[Position] = None,
    symbol: Option[Symbol] = None): ForApi = {
    TF.mkFor(inits, cond, steps, body, pos, symbol)
  }

  def mkBlock(stmts: List[Tree],
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None): BlockApi = {
    TF.mkBlock(stmts, pos, symbol)
  }

  def mkTernary(cond: Expr, thenp: Expr, elsep: Expr,
    pos: Option[Position] = None,
    tpe: Option[Type]     = None,
    owner: Option[Symbol] = None): TernaryApi = {
    TF.mkTernary(cond, thenp, elsep, pos, tpe, owner)
  }


  def mkApply(fun: Expr, args: List[Expr],
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): ApplyApi = {
    TF.mkApply(fun, args, pos, owner)
  }

  def mkReturn(expr: Option[Expr],
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): ReturnApi = {
    TF.mkReturn(expr, pos, owner)
  }



  def mkValDef(mods: Flags, tpt: UseTree, name: Name,
    rhs: Expr, pos: Option[Position] = None,
    symbol: Option[Symbol] = None): ValDefApi = {

    TF.mkValDef(mods, tpt, name, rhs, pos, symbol)
  }

  // brokenj
  def mkLabel(name: Name, stmt: Expr,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): LabelApi = {
    TF.mkLabel(name, stmt, pos, owner)
  }

  def mkBreak(label: Option[Name],
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): BreakApi = {
    TF.mkBreak(label, pos, owner)
  }

  def mkContinue(label: Option[Name],
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): ContinueApi = {
    TF.mkContinue(label, pos, owner)
  }

  def mkCase(guards: List[Expr], body: Tree,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): CaseApi = {
    TF.mkCase(guards, body, pos, owner)
  }

  def mkSwitch(expr: Expr, cases: List[CaseApi],
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): SwitchApi = {
    TF.mkSwitch(expr, cases, pos, owner)
  }
}


object TreeFactories extends TreeFactories
