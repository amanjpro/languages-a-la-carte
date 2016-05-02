package ch.usi.inf.l3.sana.oberon0.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.arrooj

import tiny.ast.{Tree, DefTree, TypeTree, TermTree, UseTree}
import tiny.ast.Implicits._
import primj.ast.{BlockApi}
import tiny.names.Name



trait TreeExtractors extends arrooj.ast.TreeExtractors {

  trait ModuleDefExtractor {
    def unappy(tree: ModuleDefApi): Option[(Name, List[DefTree], Option[BlockApi])] =
      tree match {
        case null  => None
        case _     => Some((tree.name, tree.declarations, tree.block))
      }
  }

  trait TypeDefExtractor {
    def unapply(tree: TypeDefApi): Option[(Name, UseTree)] = tree match {
      case null    => None
      case _       => Some((tree.name, tree.tpt))
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

  val CompilationUnit = new CompilationUnitExtractor {}
  val PackageDef      = new PackageDefExtractor {}
  val ClassDef        = new ClassDefExtractor {}
  val Template        = new TemplateExtractor {}
  val New             = new NewExtractor {}
  val Select          = new SelectExtractor {}
  val MethodDef       = new MethodDefExtractor {}



  val ModuleDef = new ModuleDefExtractor {}
  val TypeDef   = new TypeDefExtractor {}
}
