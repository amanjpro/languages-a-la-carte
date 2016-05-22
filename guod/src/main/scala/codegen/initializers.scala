package ch.usi.inf.l3.sana.guod.codegen



import ch.usi.inf.l3.sana
import sana.guod
import sana.tiny
import sana.ooj
import sana.primj
import sana.calcj



import tiny.dsl._
import tiny.core.TransformationComponent
import tiny.modifiers.Flags
import tiny.names.Name
import ooj.modifiers.{STATIC, STATIC_INIT}
import guod.modifiers.ModifiersUtils
import guod.ast.TreeFactories
import guod.ast.TreeExtractors._
import guod.ast._
import tiny.symbols.Symbol
import calcj.types.{LongType, DoubleType}
import guod.names.StdNames
import guod.symbols.SymbolUtils
import guod.ast.Implicits._
import ooj.modifiers.Ops._

trait InitializerComponent extends
  TransformationComponent[Tree, Tree] {
  def inline: Tree => Tree
}

// Program: DONE
// PackageDef: DONE
// ClassDef: DONE
// Template: DONE
// CompilationUnit: DONE


@component
trait ProgramInitializerComponent extends InitializerComponent {
  (program: ProgramApi) => {
    val members = program.members.map(m => inline(m))
    TreeCopiers.copyProgram(program)(members = members)
  }
}

@component
trait PackageDefInitializerComponent extends InitializerComponent {
  (pkg: PackageDefApi) => {
    val members = pkg.members.map(inline(_))
    TreeCopiers.copyPackageDef(pkg)(members = members)
  }
}

@component
trait CompilationUnitInitializerComponent extends InitializerComponent {
  (cunit: CompilationUnitApi) => {
    val module = inline(cunit.module).asInstanceOf[PackageDefApi]
    TreeCopiers.copyCompilationUnit(cunit)(module = module)
  }
}

@component
trait ClassDefInitializerComponent extends InitializerComponent {
  (clazz: ClassDefApi) => {
    val body = inline(clazz.body).asInstanceOf[TemplateApi]
    TreeCopiers.copyClassDef(clazz)(body = body)
  }
}

@component
trait TemplateInitializerComponent extends InitializerComponent {
  (template: TemplateApi) => {
    val z1: List[Tree] = Nil
    val z2: List[Tree] = Nil
    val (staticInits, instanceInits) = template.members.foldLeft((z1, z2)) {
      (z, y) => {
        y match {
          case valdef: ValDefApi                                 =>
            val id = TreeFactories.mkIdent(valdef.name, valdef.pos)
            val assign = TreeFactories.mkAssign(id, valdef.rhs, valdef.pos)
            // val res = compiler.typeCheck(valdef.owner)(assign)
            valdef.symbol.foreach(id.symbol = _)
            valdef.tpe.foreach { tpe =>
              id.tpe = tpe
              assign.tpe = tpe
            }
            valdef.owner.foreach { owner =>
              id.owner = owner
              assign.owner = owner
            }
            val p      = valdef.rhs match {
              case Literal(NullConstant)     =>
                false
              case _: LiteralApi             =>
                true
              case _                         =>
                false
            }
            if(valdef.mods.isStatic && p)
              (z._1 ++ List(assign), z._2)
            else if(!valdef.mods.isStatic)
              (z._1, z._2 ++ List(assign))
            else z
          case block: BlockApi                                   =>
            (z._1 ++ block.stmts, z._2)
          case _                                                 =>
            z
        }
      }
    }

    val noFinalInstanceInits = instanceInits.filter {
      case Assign(id, _)    =>
        id.symbol.map(!_.mods.isFinal).getOrElse(true)
      case _                =>
        true
    }

    val members    = template.members.filter(!_.isInstanceOf[BlockApi]).map {
      case mthd: MethodDefApi      if mthd.mods.isConstructor =>
        mthd.body match {
          case block: BlockApi              =>
            val body = block.stmts match {
              case (hd@Apply(Select(_: ThisApi, _), _))::rest       =>
                val stmts = hd::(noFinalInstanceInits ++ rest)
                TreeCopiers.copyBlock(block)(stmts = stmts)
              case (hd@Apply(Select(_: SuperApi, _), _))::rest      =>
                val stmts = hd::(instanceInits ++ rest)
                TreeCopiers.copyBlock(block)(stmts = stmts)
              case _                                                =>
                block
            }
            TreeCopiers.copyMethodDef(mthd)(body = body)
          case _                             =>
            mthd
        }
      case member                                             =>
          member
    }

    val clinit = {
      val mods   = STATIC | STATIC_INIT
      val ret    = TreeFactories.mkTypeUse(Name("void"), template.pos)
      val name   = StdNames.CLINIT_NAME
      val body1  = TreeFactories.mkBlock(staticInits, template.pos)
      val body2  = TreeFactories.mkBlock(Nil, template.pos)
      val res    = compiler.typeCheck(template.owner) {
        TreeFactories.mkMethodDef(mods, ret, name, Nil,
          Nil, body1, template.pos)
      }.asInstanceOf[MethodDefApi]
      TreeCopiers.copyMethodDef(res)(body = body2)
    }

    TreeCopiers.copyTemplate(template)(members = members ++ List(clinit))

  }
}
