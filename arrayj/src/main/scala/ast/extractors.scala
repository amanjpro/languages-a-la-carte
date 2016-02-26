package ch.usi.inf.l3.sana.arrayj.ast


import ch.usi.inf.l3.sana
import sana.tiny
import sana.brokenj
import sana.tiny.ast.{Tree, Expr, UseTree}


trait TreeExtractors extends brokenj.ast.TreeExtractors {
  trait ArrayInitializerExtractor {
    def unapply(tree: ArrayInitializerApi):
      Option[List[Expr]] = tree match {
      case null          => None
      case _             => Some(tree.elements)
    }
  }


  trait ArrayAccessExtractor {
    def unapply(tree: ArrayAccessApi):
      Option[(Expr, Expr)] = tree match {
      case null          => None
      case _             => Some((tree.array, tree.index))
    }
  }

  trait ArrayTypeUseExtractor {
    def unapply(tree: ArrayTypeUseApi):
      Option[UseTree] = tree match {
      case null          => None
      case _             => Some(tree.tpt)
    }
  }

  trait ArrayCreationExtractor {
    def unapply(tree: ArrayCreationApi):
      Option[(Expr, Option[Expr])] = tree match {
      case null          => None
      case _             => Some((tree.array, tree.size))
    }
  }
}

object TreeExtractors extends TreeExtractors {
  val TypeUse          = new TypeUseExtractor {}
  val Ident            = new IdentExtractor {}

  val Cast             = new CastExtractor {}
  val Literal          = new LiteralExtractor {}
  val Binary           = new BinaryExtractor {}
  val Unary            = new UnaryExtractor {}


  val Block            = new BlockExtractor {}
  val Assign           = new AssignExtractor {}
  val If               = new IfExtractor {}
  val While            = new WhileExtractor {}
  val For              = new ForExtractor {}
  val Ternary          = new TernaryExtractor {}
  val Apply            = new ApplyExtractor {}
  val Return           = new ReturnExtractor {}
  val ValDef           = new ValDefExtractor {}


  val Label            = new LabelExtractor {}
  val Break            = new BreakExtractor {}
  val Continue         = new ContinueExtractor {}
  val Case             = new CaseExtractor {}
  val Switch           = new SwitchExtractor {}

  val ArrayInitializer = new ArrayInitializerExtractor {}
  val ArrayAccess      = new ArrayAccessExtractor {}
  val ArrayTypeUse     = new ArrayTypeUseExtractor {}
  val ArrayCreation    = new ArrayCreationExtractor {}
}
