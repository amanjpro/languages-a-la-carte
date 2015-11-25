package ch.usi.inf.l3.sana.calcj.ast


import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.tiny.ast._
import tiny.names.Name
import operators._


trait TreeExtractors extends tiny.ast.TreeExtractors {


  trait CastExtractor {
    def unapply(cast: CastApi): Option[(UseTree, Expr)] = cast match {
      case null          => None
      case _             => Some((cast.tpt, cast.expr))
    }
  }


  trait LiteralExtractor {
    def unapply(lit: LiteralApi): Option[Constant] = lit match {
      case null          => None
      case _             => Some(lit.constant)
    }
  }


  trait BinaryExtractor {
    def unapply(bin: BinaryApi): Option[(Expr, BOp, Expr)] = bin match {
      case null          => None
      case _             => Some((bin.lhs, bin.op, bin.rhs))
    }
  }

  trait UnaryExtractor {
    def unapply(unary: UnaryApi): Option[(Boolean, UOp, Expr)] = unary match {
      case null          => None
      case _             => Some((unary.isPostfix, unary.op, unary.expr))
    }
  }
}


object TreeExtractors extends TreeExtractors {
  val TypeUse = new TypeUseExtractor {}
  val Ident   = new IdentExtractor {}

  val Cast    = new CastExtractor {}
  val Literal = new LiteralExtractor {}
  val Binary  = new BinaryExtractor {}
  val Unary   = new UnaryExtractor {}
}
