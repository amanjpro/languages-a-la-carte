package ch.usi.inf.l3.sana.oberon0.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.arrayj
import sana.ooj

import tiny.ast._
import tiny.ast.Implicits._
import primj.ast.{BlockApi}
import tiny.names.Name
import ooj.ast.{TemplateApi, ClassDefApi}
import arrayj.ast.{TreeExtractors => ATreeExtractors}
import ooj.ast.{TreeExtractors => OTreeExtractors}



trait TreeExtractors extends primj.ast.TreeExtractors {

  trait ModuleDefExtractor {
    def unappy(tree: ModuleDefApi): Option[(Name, List[DefTree], Option[BlockApi])] =
      tree match {
        case null  => None
        case _     => Some((tree.name, tree.declarations, tree.block))
      }
  }

  trait TypeDefExtractor {
    def unapply(tree: TypeDefApi): Option[(Name, Tree)] = tree match {
      case null    => None
      case _       => Some((tree.name, tree.tpt))
    }
  }


  trait ArrayTypeUseExtractor {
    def unapply(tree: ArrayTypeUseApi): Option[(UseTree, Expr)] = tree match {
      case null    => None
      case _       => Some((tree.tpt, tree.size))
    }
  }



  trait ArrayAccessExtractor
    extends ATreeExtractors.ArrayAccessExtractor


  trait RecordDefExtractor {
    def unapply(tree: ClassDefApi): Option[TemplateApi] = tree match {
      case null    => None
      case _       => Some(tree.body)
    }
  }

  trait TemplateExtractor
    extends OTreeExtractors.TemplateExtractor

  trait SelectExtractor
    extends OTreeExtractors.SelectExtractor
}

object TreeExtractors extends TreeExtractors {
  val TypeUse          = new TypeUseExtractor {}
  val Ident            = new IdentExtractor {}

  val Literal          = new LiteralExtractor {}
  val Binary           = new BinaryExtractor {}
  val Unary            = new UnaryExtractor {}


  val Block            = new BlockExtractor {}
  val Assign           = new AssignExtractor {}
  val If               = new IfExtractor {}
  val While            = new WhileExtractor {}
  val Apply            = new ApplyExtractor {}
  val ValDef           = new ValDefExtractor {}


  val ArrayAccess      = new ArrayAccessExtractor {}
  val ArrayTypeUse     = new ArrayTypeUseExtractor {}

  val RecordDef        = new RecordDefExtractor {}
  val Template        = new TemplateExtractor {}
  val Select          = new SelectExtractor {}
  val MethodDef       = new MethodDefExtractor {}



  val ModuleDef = new ModuleDefExtractor {}
  val TypeDef   = new TypeDefExtractor {}
}
