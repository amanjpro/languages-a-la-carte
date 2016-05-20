package ch.usi.inf.l3.sana.guod.codegen

import ch.usi.inf.l3.sana
import sana.guod
import sana.calcj
import sana.tiny
import sana.ooj
import sana.arrooj
import sana.primj

import guod.ast._
import tiny.types._
import ooj.types._
import arrooj.types._
import calcj.types._
import primj.types.VoidType

import org.objectweb.asm.{Opcodes, MethodVisitor}

trait CodegenUtils {
  def toInternalTypeRepresentation(fullName: String,
        addSemicolon: Boolean): String = fullName match {
    case "boolean"                => "Z"
    case "char"                   => "C"
    case "byte"                   => "B"
    case "short"                  => "S"
    case "int"                    => "I"
    case "float"                  => "F"
    case "long"                   => "J"
    case "double"                 => "D"
    case "void"                   => "V"
    case _                        =>
      val internalName = fullName.replaceAll("[.]", "/")
      if(addSemicolon)
        s"L$internalName;"
      else s"$internalName"
  }

  def elementTypeToOpcode(etpe: Option[Type]): Either[String, Int] =
    etpe match {
      case Some(BooleanType)         => Right(Opcodes.T_BOOLEAN)
      case Some(ByteType)            => Right(Opcodes.T_BYTE)
      case Some(CharType)            => Right(Opcodes.T_CHAR)
      case Some(ShortType)           => Right(Opcodes.T_SHORT)
      case Some(IntType)             => Right(Opcodes.T_INT)
      case Some(LongType)            => Right(Opcodes.T_LONG)
      case Some(FloatType)           => Right(Opcodes.T_FLOAT)
      case Some(DoubleType)          => Right(Opcodes.T_DOUBLE)
      case Some(tpe)                 =>
        Left(toInternalTypeRepresentation(tpe, false))
      case _                         => Left("")
    }



  def toInternalTypeRepresentation(tpe: Type,
          addSemicolon: Boolean): String = {
    if(tpe.isInstanceOf[PrimitiveType]) {
      tpe match {
        case BooleanType                => "Z"
        case CharType                   => "C"
        case ByteType                   => "B"
        case ShortType                  => "S"
        case IntType                    => "I"
        case FloatType                  => "F"
        case LongType                   => "J"
        case DoubleType                 => "D"
        case _                          => ""
      }
    } else {
      tpe match {
        case VoidType                   => "V"
        case atpe: ArrayType            =>
          val res = s"[${toInternalTypeRepresentation(
                          atpe.componentType, false)}"
          if(addSemicolon) s"$res;" else res
        case ctpe: ClassTypeApi         =>
          val res = s"${ctpe.qualifiedName.replaceAll("[.]", "/")}"
          if(addSemicolon) s"L$res;" else res
        case _                          => ""
      }
    }
  }
  var targetVersion: Int = Opcodes.V1_5


  def fullNameToInternalName(fullName: String): String =
    fullName.replaceAll("[.]", "/")


  def useTreeToInternalType(use: UseTree,
            addSemicolon: Boolean): String        = use match {
    case tuse: ArrayTypeUseApi                           =>
      s"[${useTreeToInternalType(tuse.tpt, addSemicolon)}"
    case _                                               =>
      toInternalTypeRepresentation(TreeUtils.toQualifiedString(use),
        addSemicolon)
  }


  def storeToLocalVariable(tpe: Option[Type],
        index: Int,
        stackInfo: StackInfo,
        mv: MethodVisitor,
        isArray: Boolean): Unit = {
    val opcode = tpe match {
      case Some(BooleanType)         =>
        if(isArray) Opcodes.IASTORE else Opcodes.ISTORE
      case Some(ByteType)            =>
        if(isArray) Opcodes.BASTORE else Opcodes.ISTORE
      case Some(ShortType)           =>
        if(isArray) Opcodes.SASTORE else Opcodes.ISTORE
      case Some(CharType)            =>
        if(isArray) Opcodes.CASTORE else Opcodes.ISTORE
      case Some(IntType)             =>
        if(isArray) Opcodes.IASTORE else Opcodes.ISTORE
      case Some(LongType)            =>
        stackInfo.decrementSP
        if(isArray) Opcodes.LASTORE else Opcodes.LSTORE
      case Some(FloatType)           =>
        if(isArray) Opcodes.FASTORE else Opcodes.FSTORE
      case Some(DoubleType)          =>
        stackInfo.decrementSP
        if(isArray) Opcodes.DASTORE else Opcodes.DSTORE
      case _                         =>
        if(isArray) Opcodes.AASTORE else Opcodes.ASTORE
    }
    if(isArray)
      mv.visitInsn(opcode)
    else
      mv.visitVarInsn(opcode, index)
  }


  def loadFromLocalVariable(tpe: Option[Type],
        index: Int,
        stackInfo: StackInfo, mv: MethodVisitor,
        isArray: Boolean): Unit = {
    val opcode = tpe match {
      case Some(BooleanType)         =>
        if(isArray) Opcodes.IALOAD else Opcodes.ILOAD
      case Some(ByteType)            =>
        if(isArray) Opcodes.BALOAD else Opcodes.ILOAD
      case Some(ShortType)           =>
        if(isArray) Opcodes.SALOAD else Opcodes.ILOAD
      case Some(CharType)            =>
        if(isArray) Opcodes.CALOAD else Opcodes.ILOAD
      case Some(IntType)             =>
        if(isArray) Opcodes.IALOAD else Opcodes.ILOAD
      case Some(LongType)            =>
        stackInfo.incrementSP
        if(isArray) Opcodes.LALOAD else Opcodes.LLOAD
      case Some(FloatType)           =>
        if(isArray) Opcodes.FALOAD else Opcodes.FLOAD
      case Some(DoubleType)          =>
        stackInfo.incrementSP
        if(isArray) Opcodes.DALOAD else Opcodes.DLOAD
      case _                         =>
        if(isArray) Opcodes.AALOAD else Opcodes.ALOAD
    }
    if(isArray)
      mv.visitInsn(opcode)
    else
      mv.visitVarInsn(opcode, index)
  }
}

object CodegenUtils extends CodegenUtils

