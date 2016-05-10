package ch.usi.inf.l3.sana.oberon0.codegen

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.brokenj
import sana.arrayj
import sana.ooj
import sana.oberon0

import tiny.core.TransformationComponent
import tiny.dsl._
import oberon0.ast._
import tiny.ast.{DefTree, UseTree, Expr, Tree, IdentApi, TypeUseApi}
import calcj.ast.{LiteralApi, BinaryApi, UnaryApi}
import calcj.ast.operators._
import calcj.types.{IntType, BooleanType}
import tiny.names.Name
import tiny.source.Position
import tiny.symbols.{Symbol, TypeSymbol}
import ooj.ast.{ClassDefApi, TemplateApi, SelectApi}
import arrayj.ast.ArrayAccessApi
import oberon0.errors.ErrorCodes._
import tiny.errors.ErrorReporting.{error, warning}
import oberon0.errors.ErrorCodes._
import primj.ast._
import oberon0.ast.Implicits._
import ooj.modifiers.Ops._
import oberon0.symbols._

// Program: DONE
// ModuleDef: DONE
// ClassDef: DONE
// Template: DONE
// TypeDef: DONE
// ArrayTypeUse:
// TypeUse:
// Ident: DONE
// MethodDef: DONE
// ValDef:
// Apply: DONE
// If: DONE
// While: DONE
// Block: DONE
// Assign: DONE
// ArrayAccess: DONE
// Select: DONE
// Binary: DONE
// Unary: DONE
// Literal: DONE



trait CodeGenComponent extends TransformationComponent[(Tree, Int), String] {
  def codegen: ((Tree, Int)) => String
}

@component(tree, col)
trait ProgramCodeGenComponent extends CodeGenComponent {
  (prg: ProgramApi) => {
    prg.members.map(m => codegen((m, 0))).mkString("\n\n\n")
  }
}

@component(tree, col)
trait ModuleDefCodeGenComponent extends CodeGenComponent {
  (module: ModuleDefApi) => {
    val decls = module.declarations.map( d =>
      tab(col) + codegen((d, col + TAB))).mkString("\n")
    val main = module.block.map { block =>
      s"${tab(col + TAB)}def main(args: Array[String]): Unit = ${
        codegen((block, col + TAB + TAB))}"
    }.getOrElse("")
    s"""|${tab(col)}object ${module.name.asString} {
        |$decls
        |$main
        |${tab(col)}}""".stripMargin
  }
}

@component(tree, col)
trait ClassDefCodeGenComponent extends CodeGenComponent {
  (clazz: ClassDefApi) => codegen((clazz.body, col))
}

@component(tree, col)
trait TypeDefCodeGenComponent extends CodeGenComponent {
  (tdef: TypeDefApi) => {
    val tpt = codegen((tdef.tpt, col + TAB))
    s"${tab(col)}type ${tdef.name.asString} =$tpt"
  }
}

@component(tree, col)
trait TemplateCodeGenComponent extends CodeGenComponent {
  (tmpl: TemplateApi) => {
    val members = tmpl.members.map(m => codegen((m, col + TAB))).mkString("\n")
    s"""|${tab(col)}{
        |$members
        |${tab(col)}}""".stripMargin
  }
}

@component(tree, col)
trait MethodDefCodeGenComponent extends CodeGenComponent {
  (mdef: MethodDefApi) => {
    val params = mdef.params.map(p => codegen((p, col))).mkString(", ")
    val body   = codegen((mdef.body, col + TAB))
    s"${tab(col)}def ${mdef.name.asString}($params): Unit =\n$body"
  }
}

@component(tree, col)
trait ArrayTypeUseCodeGenComponent extends CodeGenComponent {
  (tuse: ArrayTypeUseApi) => {
    "array type here"
  }
}

@component(tree, col)
trait TypeUseCodeGenComponent extends CodeGenComponent {
  (tuse: TypeUseApi) => {
    tuse.name.asString match {
      case "INTEGER"    => "Int"
      case "BOOLEAN"    => "Boolean"
      case s            => s
    }
  }
}

@component(tree, col)
trait ValDefCodeGenComponent extends CodeGenComponent {
  (valdef: ValDefApi) => {
    val keyword = if(valdef.mods.isFinal) "val" else "var"
    val tpt     = codegen((valdef.tpt, 0))
    // valdef.tpe match {
    //   case Some(t@ArrayType(t, size))      =>
    //     val sizes = getSizes.map(codegen(_)).mkString(", ")
    //     s"Array[${t.name.asString}].ofDim($size)"
    //   case _                               =>
    // }
    // ""
    val rhs = valdef.tpt.tpe match {
      case Some(IntType)             => "0"
      case Some(BooleanType)         => "false"
      case _                         => "null"
    }
    s"${tab(col)}$keyword ${valdef.name.asString}: $tpt = $rhs"
  }

  // protected def typeToString(tpe: String): String = tpe match {
  //   case "INTEGER"      => "Int"
  //   case "BOOLEAN"      => "Boolean"
  //   case "BOOLEAN"      => "Boolean"
  // }
  //
  // protected def getSizes(tpe: Type): List[Expr] = tpe match {
  //   case ArrayType(t, size)        => size::getSizes(t)
  //   case _                         => Nil
  // }
}

@component(tree, col)
trait ArrayAccessCodeGenComponent extends CodeGenComponent {
  (acc: ArrayAccessApi) => {
    val array   = codegen((acc.array, 0))
    val index   = codegen((acc.index, 0))
    s"${tab(col)}$array.apply($index)"
  }
}

@component(tree, col)
trait BlockCodeGenComponent extends CodeGenComponent {
  (block: BlockApi) => {
    val stmts   = block.stmts.map(s => codegen((s, col + TAB))).mkString("\n")
    s"""|${tab(col)}{
        |$stmts
        |${tab(col)}}""".stripMargin
  }
}

@component(tree, col)
trait SelectCodeGenComponent extends CodeGenComponent {
  (slct: SelectApi) => {
    val qual   = codegen((slct.qual, 0))
    val tree   = codegen((slct.tree, 0))
    s"${tab(col)}$qual.$tree"
  }
}

@component(tree, col)
trait ApplyCodeGenComponent extends CodeGenComponent {
  (app: ApplyApi) => {
    val args  = app.args.map(a => codegen((a, 0))).mkString(", ")
    app.fun match {
      case id: IdentApi  if id.name.asString == "Write" =>
        s"${tab(col)}print($args)"
      case id: IdentApi  if id.name.asString == "Read"  =>
        s"${tab(col)}${args.head} = scala.io.StdIn.read(${""}).asInstanceOf[${args.head}.type]"
      case _                                            =>
        val fun = codegen((app.fun, 0))
        s"${tab(col)}$fun($args)"
    }
  }
}

@component(tree, col)
trait IfCodeGenComponent extends CodeGenComponent {
  (ifelse: IfApi) => {
    val cond  = codegen((ifelse.cond, 0))
    val thenp = codegen((ifelse.thenp, col))
    val elsep = codegen((ifelse.elsep, col))
    s"${tab(col)}if($cond) $thenp\n${tab(col)}else $elsep"
  }
}

@component(tree, col)
trait WhileCodeGenComponent extends CodeGenComponent {
  (wile: WhileApi) => {
    val cond = codegen((wile.cond, 0))
    val body = codegen((wile.body, col))
    s"${tab(col)}while($cond)\n$body"
  }
}

@component(tree, col)
trait AssignCodeGenComponent extends CodeGenComponent {
  (assign: AssignApi) => {
    val lhs = codegen((assign.lhs, 0))
    val rhs = codegen((assign.rhs, 0))
    s"${tab(col)}$lhs = $rhs"
  }
}

@component(tree, col)
trait BinaryCodeGenComponent extends CodeGenComponent {
  (bin: BinaryApi) => {
    val op   = bin.op match {
      case Mul         => "*"
      case Div         => "/"
      case Mod         => "%"
      case And         => "&&"
      case Or          => "||"
      case Add         => "+"
      case Sub         => "-"
      case Eq          => "=="
      case Neq         => "!="
      case Lt          => "<"
      case Le          => "<="
      case Gt          => ">"
      case Ge          => ">="
    }
    val lhs = codegen((bin.lhs, 0))
    val rhs = codegen((bin.rhs, 0))
    s"${tab(col)}($lhs $op $rhs)"
  }
}

@component(tree, col)
trait UnaryCodeGenComponent extends CodeGenComponent {
  (unary: UnaryApi) => {
    val op   = unary.op match {
      case Not         => "!"
      case Pos         => "+"
      case Neg         => "-"
    }
    val expr = codegen((unary.expr, 0))
    s"${tab(col)}($op $expr)"
  }
}

@component(tree, col)
trait IdentCodeGenComponent extends CodeGenComponent {
  (ident: IdentApi) => {
    val name = ident.name.asString match {
      case "WriteLn"       => "println(\"\")"
      case name            => name
    }
    s"${tab(col)}$name"
  }
}

@component(tree, col)
trait LiteralCodeGenComponent extends CodeGenComponent {
  (lit: LiteralApi) => s"${tab(col)}${lit.constant.value.toString}"
}
