package ch.usi.inf.l3.sana.brokenj.ast


import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import sana.tiny.ast._
import tiny.names.Name


trait TreeExtractors extends primj.ast.TreeExtractors {


  trait LabelExtractor {
    def unapply(tree: LabelApi): Option[(Name, Expr)] = tree match {
      case null          => None
      case _             => Some((tree.name, tree.stmt))
    }
  }

  trait BreakExtractor {
    def unapply(tree: BreakApi): Option[Option[Name]] = tree match {
      case null          => None
      case _             => Some(tree.label)
    }
  }

  trait ContinueExtractor {
    def unapply(tree: ContinueApi): Option[Option[Name]] = tree match {
      case null          => None
      case _             => Some(tree.label)
    }
  }


  trait CaseExtractor {
    def unapply(tree: CaseApi): Option[(List[Expr], Tree)] = tree match {
      case null          => None
      case _             => Some((tree.guards, tree.body))
    }
  }


  trait SwitchExtractor {
    def unapply(tree: SwitchApi): Option[(Expr, List[CaseApi])] =
      tree match {
        case null          => None
        case _             => Some((tree.expr, tree.cases))
      }
  }

}


object TreeExtractors extends TreeExtractors {
  val TypeUse   = new TypeUseExtractor {}
  val Ident     = new IdentExtractor {}

  val Cast      = new CastExtractor {}
  val Literal   = new LiteralExtractor {}
  val Binary    = new BinaryExtractor {}
  val Unary     = new UnaryExtractor {}


  val Program   = new ProgramExtractor {}
  val Assign    = new AssignExtractor {}
  val If        = new IfExtractor {}
  val While     = new WhileExtractor {}
  val For       = new ForExtractor {}
  val Ternary   = new TernaryExtractor {}
  val Apply     = new ApplyExtractor {}
  val Return    = new ReturnExtractor {}
  val MethodDef = new MethodDefExtractor {}
  val ValDef    = new ValDefExtractor {}


  val Label     = new LabelExtractor {}
  val Break     = new BreakExtractor {}
  val Continue  = new ContinueExtractor {}
  val Case      = new CaseExtractor {}
  val Switch    = new SwitchExtractor {}

}
