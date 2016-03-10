package ch.usi.inf.l3.sana.robustj.ast


import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import sana.brokenj
import sana.ooj
import sana.arrayj
import sana.arrooj
import sana.robustj
import sana.tiny.ast._
import sana.primj.ast.{ValDefApi, BlockApi}
import tiny.names.Name
import tiny.modifiers.Flags


trait TreeExtractors extends arrooj.ast.TreeExtractors {

  trait ThrowExtractor {
    def unapply(tree: ThrowApi): Option[Expr] = tree match {
        case null          => None
        case _             =>
          Some(tree.expr)
      }
  }

  trait TryExtractor {
    def unapply(tree: TryApi):
      Option[(BlockApi, List[CatchApi], Option[BlockApi])] = tree match {
        case null              => None
        case _                 =>
          Some((tree.tryClause, tree.catches, tree.finallyClause))
      }
  }

  trait CatchExtractor {
    def unapply(tree: CatchApi): Option[(ValDefApi, BlockApi)] = tree match {
      case null              => None
      case _                 =>
        Some((tree.eparam, tree.catchClause))
    }
  }

  trait MethodDefExtractor {
    def unapply(tree: MethodDefApi):
      Option[(Flags, UseTree, Name, List[ValDefApi], List[UseTree], Expr)] =
        tree match {
          case null          => None
          case _             =>
            Some((tree.mods, tree.ret, tree.name, tree.params,
              tree.throwsClause, tree.body))
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
}
