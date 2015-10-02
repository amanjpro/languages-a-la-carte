package ch.usi.inf.l3.sana.test.tyeprs


import org.scalatest._
import ch.usi.inf.l3.sana
import sana.core._
import sana.dsl._
import sana.core.Implicits._
import sana.tiny.ast._
import sana.tiny.types._
import sana.calcj.ast._
import sana.calcj.ast.operators._
import sana.calcj.types._
import sana.calcj.typechecker._



class TyperTest extends FlatSpec with Matchers {

  final val langs = "Binary,Unary,Literal,Cast"



  // Transformation Family #1
  trait TyperFamily extends TransformationFamily[Tree, Tree] {
    self =>

    def default: Tree = NoTree

    def components: List[PartialFunction[Tree, Tree]] =
      generateComponents[Tree, Tree](langs,
        "TyperComponent", "typed")
    def typed: Tree => Tree = family
  }



  case object NoType extends Type {
    def =:=(other: Type): Boolean = other == NoType
    def <:<(other: Type): Boolean = other =:= this
  }

  def getTpe(tree: Tree): Type = tree.tpe.getOrElse(NoType)

  val typer = new TyperFamily {}

  "1L >> 2" should "Long" in {
    val b = Binary(
              Literal(LongConstant(1), None),
              SHR,
              Literal(IntConstant(2), None),
              None,
              None,
              None)
    val tpe = getTpe(typer.typed(b))
    (tpe =:= LongType) should be (true)
  }

  "(short) 1 >>> 1L" should "Int" in {
    val b = Binary(
              Literal(ShortConstant(1), None),
              USHR,
              Literal(LongConstant(1), None),
              None,
              None,
              None)
    val tpe = getTpe(typer.typed(b))
    (tpe =:= IntType) should be (true)
  }

  "1 << 1.0f" should "not type check" in {
    val b = Binary(
              Literal(IntConstant(1), None),
              SHL,
              Literal(FloatConstant(1.0f), None),
              None,
              None,
              None)
    val tpe = getTpe(typer.typed(b))
    (tpe == NoType) should be (true)
  }

  "(short) 1 >>> true" should "not type check" in {
    val b = Binary(
              Literal(ShortConstant(1), None),
              USHR,
              Literal(BooleanConstant(true), None),
              None,
              None,
              None)
    val tpe = getTpe(typer.typed(b))
    (tpe == NoType) should be (true)
  }

  "(short) 1 + -((byte) 2) * 4" should "be int" in {
    val b = Binary(
              Literal(ShortConstant(1), None),
              Add,
              Binary(
                Unary(false, Neg,
                  Literal(ByteConstant(2), None),
                  None,
                  None,
                  None),
                Mul,
                Literal(IntConstant(4), None),
                None,
                None,
                None),
              None,
              None,
              None)
    val tpe = getTpe(typer.typed(b))
    (tpe =:= IntType) should be (true)
  }


  "-('a') * 2.2D" should "be double" in {
    val b = Binary(
              Unary(false,
                Neg,
                Literal(CharConstant('a'), None),
                None,
                None,
                None),
              Mul,
              Literal(DoubleConstant(2.2D), None),
              None,
              None,
              None
            )
    val tpe = getTpe(typer.typed(b))
    (tpe =:= DoubleType) should be (true)
  }

  "(short) 1 + (byte) 2 + 2.5" should "be Double" in {
    val b = Binary(
              Literal(ShortConstant(1), None),
              Add,
              Binary(
                Literal(ByteConstant(2), None),
                Add,
                Literal(DoubleConstant(2.5), None),
                None,
                None,
                None),
              None,
              None,
              None)
    val tpe = getTpe(typer.typed(b))
    (tpe =:= DoubleType) should be (true)
  }

  "+((short) 1)" should "be int" in {
    val b = Unary(false,
              Pos,
              Literal(ByteConstant(1), None),
              None,
              None,
              None)
    val tpe = getTpe(typer.typed(b))
    (tpe =:= IntType) should be (true)
  }

}
