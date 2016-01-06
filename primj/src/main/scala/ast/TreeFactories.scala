package ch.usi.inf.l3.sana.primj.ast

import ch.usi.inf.l3.sana
import sana.tiny.types.Type
import sana.tiny.source.Position
import sana.tiny.symbols.Symbol
import sana.tiny.names.Name
import sana.primj.ast.Implicits._
import sana.tiny.modifiers.Flags
import sana.tiny.ast._
import sana.calcj.ast._
import sana.primj.ast._
import sana.primj.types._



trait TreeFactories extends sana.calcj.ast.TreeFactories {

  def mkProgram(members: List[DefTree], sourceName: String,
              symbol: Option[Symbol] = None): ProgramApi = {
    val res = new Program(members, sourceName)
    symbol.foreach( sym => {
      res.symbol = sym
      sym.owner.foreach(res.owner = _)
    })
    res
  }


  def mkAssign(lhs: Expr, rhs: Expr,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): AssignApi = {
    val res = new Assign(lhs, rhs)
    pos.foreach(res.pos = _)
    owner.foreach(res.owner = _)
    lhs.tpe.foreach(res.tpe = _)
    res
  }


  def mkIf(cond: Expr, thenp: Expr, elsep: Expr,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): IfApi = {
    val res = new If(cond, thenp, elsep)
    pos.foreach(res.pos = _)
    owner.foreach(res.owner = _)
    res.tpe = VoidType
    res
  }


  def mkWhile(isDoWhile: Boolean, cond: Expr, body: Expr,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): WhileApi = {
    val res = new While(isDoWhile, cond, body)
    pos.foreach(res.pos = _)
    owner.foreach(res.owner = _)
    res.tpe = VoidType
    res
  }

  def mkFor(inits: List[Tree], cond: Expr, steps: List[Expr],
    body: Expr, pos: Option[Position] = None,
    symbol: Option[Symbol] = None): ForApi = {
    val res = new For(inits, cond, steps, body)
    pos.foreach(res.pos = _)
    symbol.foreach( sym => {
      res.symbol = sym
      sym.owner.foreach(res.owner = _)
    })
    res.tpe = VoidType
    res
  }
  def mkBlock(stmts: List[Tree],
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None): BlockApi = {
    val res = new Block(stmts)
    pos.foreach(res.pos = _)
    symbol.foreach( sym => {
      res.symbol = sym
      sym.owner.foreach(res.owner = _)
    })
    stmts match {
      case Nil    =>
        res.tpe = VoidType
      case _      =>
        stmts.last.tpe.foreach(res.tpe = _)
    }
    res
  }

  def mkTernary(cond: Expr, thenp: Expr, elsep: Expr,
    pos: Option[Position] = None,
    tpe: Option[Type]     = None,
    owner: Option[Symbol] = None): TernaryApi = {
    val res = new Ternary(cond, thenp, elsep)
    pos.foreach(res.pos = _)
    owner.foreach(res.owner = _)
    tpe.foreach(res.tpe = _)
    res
  }


  def mkApply(fun: Expr, args: List[Expr],
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): ApplyApi = {

    val res = new Apply(fun, args)
    pos.foreach(res.pos = _)
    owner.foreach(res.owner = _)
    fun.tpe match {
      case Some(MethodType(r, _)) =>
        res.tpe = r
      case _                      =>
        ()
    }
    res
  }

  def mkReturn(expr: Option[Expr],
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): ReturnApi = {
    val res = new Return(expr)
    pos.foreach(res.pos = _)
    owner.foreach(res.owner = _)
    expr.flatMap(_.tpe).foreach(res.tpe = _)
    res
  }


  def mkMethodDef(ret: UseTree,
    name: Name, params: List[ValDefApi],
    body: Expr, pos: Option[Position] = None,
    symbol: Option[Symbol] = None): MethodDefApi = {
    val res = new MethodDef(ret, name, params, body)
    pos.foreach(res.pos = _)
    symbol.foreach( sym => {
      res.symbol = sym
      sym.owner.foreach(res.owner = _)
    })
    val tys = params.flatMap(_.tpe)
    ret.tpe.foreach(t => res.tpe = MethodType(t, tys))
    res
  }

  def mkValDef(mods: Flags, tpt: UseTree, name: Name,
    rhs: Expr, pos: Option[Position] = None,
    symbol: Option[Symbol] = None): ValDefApi = {

    val res = new ValDef(mods, tpt, name, rhs)
    pos.foreach(res.pos = _)
    symbol.foreach( sym => {
      res.symbol = sym
      sym.owner.foreach(res.owner = _)
    })
    tpt.tpe.foreach(res.tpe = _)
    res
  }

}

object TreeFactories extends TreeFactories
