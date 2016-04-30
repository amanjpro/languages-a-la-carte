package ch.usi.inf.l3.usi.test.macros

import ch.usi.inf.l3.sana
import sana.tiny.dsl._
import sana.tiny.core._
import sana.tiny.core.Implicits._

/*
Nate's comments:

1- Add a case for default action, i.e. when a syntax component is missing
2- A macro that generates boilerplate, for example define a list
   of all AST trees once, then tell it what the transformation
   components are named, and the macro generates the instances
   for you.
3- Try to implement a language that needs name resolution with
   this approach.
*/
// Syntax
trait Expr {
  def tpe: Type
}

case object NoExpr extends Expr {
  def tpe: Type = ErrorType
}

case class IntLit(value: Int) extends Expr {
  val tpe: Type = IntType
}

case class Add(lhs: Expr, rhs: Expr, tpe: Type = NoType) extends Expr
case class Mul(lhs: Expr, rhs: Expr, tpe: Type = NoType) extends Expr


trait Type
case object ErrorType extends Type
case object NoType extends Type
case object IntType extends Type


// Transformation Component #1
trait TypeCheckerComponent extends TransformationComponent[Expr, Expr] {
  def typeCheck: Expr => Expr
}

@component
trait IntLitTypeCheckerComponent extends TypeCheckerComponent {
  (lit: IntLit)          => lit
}

@component
trait AddTypeCheckerComponent extends TypeCheckerComponent {
  (add: Add)         => {
    val nl = typeCheck(add.lhs)
    val nr = typeCheck(add.rhs)
    val ty1 = nl.tpe
    val ty2 = nr.tpe
    if(ty1 == IntType && ty2 == IntType)
      Add(nl, nr, IntType)
    else
      Add(nl, nr, ErrorType)
  }
}

@component
trait MulTypeCheckerComponent extends TypeCheckerComponent {
  (mul: Mul)         => {
    val nl = typeCheck(mul.lhs)
    val nr = typeCheck(mul.rhs)
    val ty1 = nl.tpe
    val ty2 = nr.tpe
    if(ty1 == IntType && ty2 == IntType)
      Mul(nl, nr, IntType)
    else
      Mul(nl, nr, ErrorType)
  }
}

// Transformation Component #2
trait PrettyPrinterComponent extends TransformationComponent[Expr, String] {
  def pprint: Expr => String
}

@component
trait IntLitPrettyPrinterComponent extends PrettyPrinterComponent {
  (lit: IntLit)          => lit.value.toString
}

@component
trait AddPrettyPrinterComponent extends PrettyPrinterComponent {
  (add: Add)         => {
    val sl = pprint(add.lhs)
    val sr = pprint(add.rhs)
    s"($sl + $sr)"
  }
}

@component
trait MulPrettyPrinterComponent extends PrettyPrinterComponent {
  (mul: Mul)         => {
    val sl = pprint(mul.lhs)
    val sr = pprint(mul.rhs)
    s"($sl * $sr)"
  }
}


// Third family, checker
trait TestCheckerComponent extends CheckerComponent[Expr] {
  def check: Expr => Unit
}

@component
trait IntLitTestCheckerComponent extends TestCheckerComponent {
  (lit: IntLit)          => println(lit.value.toString)
}

@component
trait AddTestCheckerComponent extends TestCheckerComponent {
  (add: Add)         => {
    val sl = check(add.lhs)
    val sr = check(add.rhs)
    println(s"($sl + $sr)")
  }
}

@component
trait MulTestCheckerComponent extends TestCheckerComponent {
  (mul: Mul)         => {
    val sl = check(mul.lhs)
    val sr = check(mul.rhs)
    println(s"($sl * $sr)")
  }
}


// Complex langs
object ComplexLangs {
  final val langs = "IntLit,Add,Mul"
}



// Transformation Family #1
trait TypeCheckerFamily extends TransformationFamily[Expr, Expr] {
  self =>

  def compiler: CompilerInterface = ???

  def components: List[PartialFunction[Expr, Expr]] =
    generateComponents[Expr, Expr](ComplexLangs.langs,
      "TypeCheckerComponent", "typeCheck", "")
  def typeCheck: Expr => Expr = family
}

object TypeCheckerFamily extends TypeCheckerFamily

// Transformation Family #2
trait PrettyPrinterFamily extends TransformationFamily[Expr, String] {
  self =>

  def compiler: CompilerInterface = ???
  def components: List[PartialFunction[Expr, String]] =
    generateComponents[Expr, String](ComplexLangs.langs,
      "PrettyPrinterComponent", "pprint", "")

  def pprint: Expr => String = family
}

object PrettyPrinterFamily extends PrettyPrinterFamily

// Checker Family #1
trait TestCheckerFamily extends CheckerFamily[Expr] {
  self =>

  def compiler: CompilerInterface = ???

  def components: List[PartialFunction[Expr, Unit]] =
    generateComponents[Expr, Unit](ComplexLangs.langs,
      "TestCheckerComponent", "check", "")

  def check: Expr => Unit = family
}

object TestCheckerFamily extends TestCheckerFamily



// A complex language module
trait ComplexExprLang extends LanguageModule[Expr, String] {

  def compiler: CompilerInterface = ???
  def compile: Expr => String = {
    TypeCheckerFamily.typeCheck join TestCheckerFamily.check join PrettyPrinterFamily.pprint
  }
}
object ComplexExprLang extends ComplexExprLang


// object SimpleLang {
//   final val langs = "Add, IntLit"
// }

// Make the language simpler (reduce its elements)
trait SimpleTypeCheckerFamily extends TypeCheckerFamily {
  self =>

  override def components: List[PartialFunction[Expr, Expr]] =
    generateComponents[Expr, Expr](ComplexLangs.langs,
      "TypeCheckerComponent", "typeCheck", "Mul")
}

object SimpleTypeCheckerFamily extends SimpleTypeCheckerFamily

trait SimplePrettyPrinterFamily extends PrettyPrinterFamily {
  self =>
  override val components = {
    generateComponents[Expr, String](ComplexLangs.langs,
      "PrettyPrinterComponent", "pprint", "Mul")
  }


  // def pprint: Expr => String = ???
}

object SimplePrettyPrinterFamily extends SimplePrettyPrinterFamily


trait SimpleExprLang extends LanguageModule[Expr, String] {

  def compile: Expr => String = {
    SimpleTypeCheckerFamily.typeCheck join SimplePrettyPrinterFamily.pprint
  }
}
object SimpleExprLang extends SimpleExprLang





// object Main {
//   def main(args: Array[String]): Unit = {
//     // Testing complex lang
//     val expr1 = Add(Mul(IntLit(1), IntLit(2)), IntLit(3))
//     val res   = ComplexExprLang.compile(expr1)
//     println(expr1)
//     println("Evaluates to...")
//     println(res)
//     // Testing simple
//     // Testing complex lang
//     val expr2 = Add(Add(IntLit(1), IntLit(2)), IntLit(3))
//     val res1 = SimpleExprLang.compile(expr2)
//     val res2 = ComplexExprLang.compile(expr2)
//     println(expr2)
//     println("Using simple lang, evaluates to...")
//     println(res1)
//     println("Using complex lang, evaluates to...")
//     println(res2)
//
//   }
// }
