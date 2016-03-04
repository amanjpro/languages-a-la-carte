package ch.usi.inf.l3.sana.arrooj.ast


import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import sana.brokenj
import sana.ooj
import sana.arrayj
import sana.tiny.ast._
import tiny.names.Name
import tiny.modifiers.Flags
import primj.ast.{ValDefApi, ApplyApi}
import arrayj.ast.{TreeExtractors => ATreeExtractors}


trait TreeExtractors extends ooj.ast.TreeExtractors {

  trait ArrayInitializerExtractor
    extends ATreeExtractors.ArrayInitializerExtractor

  trait ArrayAccessExtractor
    extends ATreeExtractors.ArrayAccessExtractor

  trait ArrayTypeUseExtractor
    extends ATreeExtractors.ArrayTypeUseExtractor

  trait ArrayCreationExtractor
    extends ATreeExtractors.ArrayCreationExtractor
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

  val CompilationUnit = new CompilationUnitExtractor {}
  val PackageDef      = new PackageDefExtractor {}
  val ClassDef        = new ClassDefExtractor {}
  val Template        = new TemplateExtractor {}
  val New             = new NewExtractor {}
  val Select          = new SelectExtractor {}
  val MethodDef       = new MethodDefExtractor {}

}
