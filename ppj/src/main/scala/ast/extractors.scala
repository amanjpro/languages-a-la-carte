package ch.usi.inf.l3.sana.ppj.ast


import ch.usi.inf.l3.sana
import sana.robustj
import sana.tiny
import sana.primj

import tiny.ast.Expr
import primj.ast.BlockApi

trait TreeExtractors extends robustj.ast.TreeExtractors {

  trait SynchronizedExtractor {
    def unapply(tree: SynchronizedApi): Option[(Expr, BlockApi)] = tree match {
        case null          => None
        case _             =>
          Some((tree.expr, tree.block))
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

  val CompilationUnit  = new CompilationUnitExtractor {}
  val PackageDef       = new PackageDefExtractor {}
  val ClassDef         = new ClassDefExtractor {}
  val Template         = new TemplateExtractor {}
  val New              = new NewExtractor {}
  val Select           = new SelectExtractor {}


  val Try              = new TryExtractor {}
  val Throw            = new ThrowExtractor {}
  val Catch            = new CatchExtractor {}
  val MethodDef        = new MethodDefExtractor {}

  val Synchronized     = new SynchronizedExtractor {}
}
