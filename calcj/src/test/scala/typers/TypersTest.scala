package ch.usi.inf.l3.sana.test.tyeprs


import org.scalatest._
import ch.usi.inf.l3.sana
import sana.core._
import sana.dsl._
import sana.core.Implicits._
import sana.tiny.symbols.Symbol
import sana.tiny.ast._
import sana.tiny.ast.Implicits._
import sana.tiny.types._
import sana.calcj.ast._
import sana.calcj.ast.operators._
import sana.calcj.types._
import sana.calcj.typechecker._
import sana.calcj.ast.TreeFactories._



class TyperTest extends FlatSpec with Matchers {

  final val langs = "Binary,Unary,Literal,Cast"



  // Transformation Family #1
  trait TyperFamily extends TransformationFamily[(Tree, List[Symbol]), Tree] {
    self =>

    override def default: Tree = NoTree

    def components: List[PartialFunction[(Tree, List[Symbol]), Tree]] =
      generateComponents[(Tree, List[Symbol]), Tree](langs,
        "TyperComponent", "typed", "")
    def typed: ((Tree, List[Symbol])) => Tree = family
  }



  case object NoType extends Type {
    def =:=(other: Type): Boolean = other == NoType
    def <:<(other: Type): Boolean = other =:= this
  }

  def getTpe(tree: Tree): Type = tree.tpe.getOrElse(NoType)

  val typer = new TyperFamily {}

  "1L >> 2" should "Long" in {
    val b = mkBinary(
              mkLiteral(LongConstant(1)),
              SHR,
              mkLiteral(IntConstant(2)))
    val tpe = getTpe(typer.typed((b, Nil)))
    (tpe =:= LongType) should be (true)
  }

  "(short) 1 >>> 1L" should "Int" in {
    val b = mkBinary(
              mkLiteral(ShortConstant(1)),
              USHR,
              mkLiteral(LongConstant(1)))
    val tpe = getTpe(typer.typed((b, Nil)))
    (tpe =:= IntType) should be (true)
  }

  "1 << 1.0f" should "not type check" in {
    val b = mkBinary(
              mkLiteral(IntConstant(1)),
              SHL,
              mkLiteral(FloatConstant(1.0f)))
    val tpe = getTpe(typer.typed((b, Nil)))
    (tpe == NoType) should be (true)
  }

  "(short) 1 >>> true" should "not type check" in {
    val b = mkBinary(
              mkLiteral(ShortConstant(1)),
              USHR,
              mkLiteral(BooleanConstant(true)))
    val tpe = getTpe(typer.typed((b, Nil)))
    (tpe == NoType) should be (true)
  }

  "(short) 1 + -((byte) 2) * 4" should "be int" in {
    val b = mkBinary(
              mkLiteral(ShortConstant(1)),
              Add,
              mkBinary(
                mkUnary(false, Neg,
                  mkLiteral(ByteConstant(2))),
                Mul,
                mkLiteral(IntConstant(4))))
    val tpe = getTpe(typer.typed((b, Nil)))
    (tpe =:= IntType) should be (true)
  }


  "-('a') * 2.2D" should "be double" in {
    val b = mkBinary(
              mkUnary(false,
                Neg,
                mkLiteral(CharConstant('a'))),
              Mul,
              mkLiteral(DoubleConstant(2.2D)))
    val tpe = getTpe(typer.typed((b, Nil)))
    (tpe =:= DoubleType) should be (true)
  }

  "(short) 1 + (byte) 2 + 2.5" should "be Double" in {
    val b = mkBinary(
              mkLiteral(ShortConstant(1)),
              Add,
              mkBinary(
                mkLiteral(ByteConstant(2)),
                Add,
                mkLiteral(DoubleConstant(2.5))))
    val tpe = getTpe(typer.typed((b, Nil)))
    (tpe =:= DoubleType) should be (true)
  }

  "+((short) 1)" should "be int" in {
    val b = mkUnary(false,
              Pos,
              mkLiteral(ByteConstant(1)))
    val tpe = getTpe(typer.typed((b, Nil)))
    (tpe =:= IntType) should be (true)
  }

}
