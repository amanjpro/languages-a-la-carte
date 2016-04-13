package ch.usi.inf.l3.usi.test

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

trait IntLitTypeCheckerComponent extends TypeCheckerComponent {

  def apply(expr: Expr): Expr = expr match {
    case lit: IntLit          => lit
  }

  def isDefinedAt(expr: Expr): Boolean   = expr match {
    case lit: IntLit          => true
    case _                 => false
  }
}

trait AddTypeCheckerComponent extends TypeCheckerComponent {

  def apply(expr: Expr): Expr = expr match {
    case Add(l, r, _)         =>
      val nl = typeCheck(l)
      val nr = typeCheck(r)
      val ty1 = nl.tpe
      val ty2 = nr.tpe
      if(ty1 == IntType && ty2 == IntType)
        Add(nl, nr, IntType)
      else
        Add(nl, nr, ErrorType)
  }


  def isDefinedAt(expr: Expr): Boolean   = expr match {
    case add: Add          => true
    case _                 => false
  }
}

trait MulTypeCheckerComponent extends TypeCheckerComponent {

  def apply(expr: Expr): Expr = expr match {
    case Mul(l, r, _)         =>
      val nl = typeCheck(l)
      val nr = typeCheck(r)
      val ty1 = nl.tpe
      val ty2 = nr.tpe
      if(ty1 == IntType && ty2 == IntType)
        Mul(nl, nr, IntType)
      else
        Mul(nl, nr, ErrorType)
  }


  def isDefinedAt(expr: Expr): Boolean   = expr match {
    case mul: Mul          => true
    case _                 => false
  }
}

// Transformation Component #2
trait PrettyPrinterComponent extends TransformationComponent[Expr, String] {
  def pprint: Expr => String
}

trait IntLitPrettyPrinterComponent extends PrettyPrinterComponent {

  def apply(expr: Expr): String = expr match {
    case IntLit(v)          => v.toString
  }

  def isDefinedAt(expr: Expr): Boolean   = expr match {
    case lit: IntLit          => true
    case _                 => false
  }
}

trait AddPrettyPrinterComponent extends PrettyPrinterComponent {

  def apply(expr: Expr): String = expr match {
    case Add(l, r, _)         =>
      val sl = pprint(l)
      val sr = pprint(r)
      s"($sl + $sr)"
  }

  def isDefinedAt(expr: Expr): Boolean   = expr match {
    case add: Add          => true
    case _                 => false
  }
}

trait MulPrettyPrinterComponent extends PrettyPrinterComponent {

  def apply(expr: Expr): String = expr match {
    case Mul(l, r, _)         =>
      val sl = pprint(l)
      val sr = pprint(r)
      s"($sl * $sr)"
  }

  def isDefinedAt(expr: Expr): Boolean   = expr match {
    case mul: Mul          => true
    case _                 => false
  }
}


// Third family, checker
trait TestCheckerComponent extends CheckerComponent[Expr] {
  def check: Expr => Unit
}

trait IntLitTestChecerComponent extends TestCheckerComponent {

  def apply(expr: Expr): Unit = expr match {
    case IntLit(v)          => println(v.toString)
  }

  def isDefinedAt(expr: Expr): Boolean   = expr match {
    case lit: IntLit          => true
    case _                 => false
  }
}

trait AddTestCheckerComponent extends TestCheckerComponent {

  def apply(expr: Expr): Unit = expr match {
    case Add(l, r, _)         =>
      val sl = check(l)
      val sr = check(r)
      println(s"($sl + $sr)")
  }

  def isDefinedAt(expr: Expr): Boolean   = expr match {
    case add: Add          => true
    case _                 => false
  }
}

trait MulTestCheckerComponent extends TestCheckerComponent {

  def apply(expr: Expr): Unit = expr match {
    case Mul(l, r, _)         =>
      val sl = check(l)
      val sr = check(r)
      println(s"($sl * $sr)")
  }

  def isDefinedAt(expr: Expr): Boolean   = expr match {
    case mul: Mul          => true
    case _                 => false
  }
}





// Transformation Family #1
trait TypeCheckerFamily extends TransformationFamily[Expr, Expr] {
  self =>

  override def default: Expr = NoExpr

  def components: List[PartialFunction[Expr, Expr]] =
    List(new IntLitTypeCheckerComponent {
           def typeCheck: Expr => Expr = self.typeCheck
           def compiler: CompilerInterface = ???
         },
         new AddTypeCheckerComponent{
           def typeCheck: Expr => Expr = self.typeCheck
           def compiler: CompilerInterface = ???
         },
         new MulTypeCheckerComponent{
           def typeCheck: Expr => Expr = self.typeCheck
           def compiler: CompilerInterface = ???
         })


  def typeCheck: Expr => Expr = family
  def compiler: CompilerInterface = ???
}

object TypeCheckerFamily extends TypeCheckerFamily

// Transformation Family #2
trait PrettyPrinterFamily extends TransformationFamily[Expr, String] {
  self =>

  def compiler: CompilerInterface = ???
  def components: List[PartialFunction[Expr, String]] =
    List(new IntLitPrettyPrinterComponent {
           def pprint: Expr => String = self.pprint
           def compiler: CompilerInterface = ???
         },
         new AddPrettyPrinterComponent{
           def pprint: Expr => String = self.pprint
           def compiler: CompilerInterface = ???
         },
         new MulPrettyPrinterComponent{
           def pprint: Expr => String = self.pprint
           def compiler: CompilerInterface = ???
         })

  def pprint: Expr => String = family
}

object PrettyPrinterFamily extends PrettyPrinterFamily

// Checker Family #1
trait TestCheckerFamily extends CheckerFamily[Expr] {
  self =>

  override def default: Unit = println("Tree not supported")

  def components: List[PartialFunction[Expr, Unit]] =
    List(new IntLitTestChecerComponent {
           def check: Expr => Unit = self.check
           def compiler: CompilerInterface = ???
         },
         new AddTestCheckerComponent{
           def check: Expr => Unit = self.check
           def compiler: CompilerInterface = ???
         },
         new MulTestCheckerComponent{
           def check: Expr => Unit = self.check
           def compiler: CompilerInterface = ???
         })

  def check: Expr => Unit = family
  def compiler: CompilerInterface = ???
}

object TestCheckerFamily extends TestCheckerFamily



// A complex language module
trait ComplexExprLang extends LanguageModule[Expr, String] {

  def compile: Expr => String = {
    TypeCheckerFamily.typeCheck join TestCheckerFamily.check join PrettyPrinterFamily.pprint
  }
}
object ComplexExprLang extends ComplexExprLang


// Make the language simpler (reduce its elements)
trait SimpleTypeCheckerFamily extends TypeCheckerFamily {
  self =>

  override def components: List[PartialFunction[Expr, Expr]] =
    List(new IntLitTypeCheckerComponent {
           def typeCheck: Expr => Expr = self.typeCheck
           def compiler: CompilerInterface = ???
         },
         new AddTypeCheckerComponent{
           def typeCheck: Expr => Expr = self.typeCheck
           def compiler: CompilerInterface = ???
         })
}

object SimpleTypeCheckerFamily extends SimpleTypeCheckerFamily

trait SimplePrettyPrinterFamily extends PrettyPrinterFamily {
  self =>
  private[this] final val CMP = "IntLit,Add" //Array("IntLit", "Add")
  override val components = {
    generateComponents[Expr, String](CMP,
      "PrettyPrinterComponent", "pprint", "")
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
