/*
 * Copyright (c) <2015-2016>, see CONTRIBUTORS
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of the <organization> nor the
 *     names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package ch.usi.inf.l3.sana.guod.codegen

import ch.usi.inf.l3.sana
import sana.guod
import sana.calcj
import sana.tiny
import sana.ooj
import sana.arrooj
import sana.primj
import sana.modulej

import guod.ast._
import tiny.types._
import ooj.types._
import arrooj.types._
import calcj.types._
import modulej.modifiers.Ops._
import modulej.ast.TreeUtils
import guod.ast.Implicits._
import guod.symbols.SymbolUtils
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
      if(addSemicolon && isNotPrimitiveTypeArray(internalName)) {
        s"L$internalName;"
      } else s"$internalName"
  }


  def isNotPrimitiveTypeArray(tpe: String): Boolean = {
    val elemTpe = tpe.filter(_ != '[')
    elemTpe != "Z" &&
      elemTpe != "C" &&
        elemTpe != "B" &&
          elemTpe != "S" &&
            elemTpe != "I" &&
              elemTpe != "J" &&
                elemTpe != "F" &&
                  elemTpe != "D"
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


  def duplicateCode(expr: Expr): Int = expr.tpe match {
    case Some(LongType) | Some(DoubleType)           =>
      Opcodes.DUP2
    case Some(atpe: ArrayType)                       =>
      if(atpe.componentType =:= DoubleType ||
          atpe.componentType =:= LongType) Opcodes.DUP2_X2
      else Opcodes.DUP2_X1
    case _                                           =>
      Opcodes.DUP
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
          s"[${toInternalTypeRepresentation(
                          atpe.componentType, true)}"
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
        mv: MethodVisitor,
        isArray: Boolean): Unit = {
    val opcode = storingToLocalVariableOpcode(tpe, isArray)
    if(isArray)
      mv.visitInsn(opcode)
    else
      mv.visitVarInsn(opcode, index)
  }


  def storingToLocalVariableOpcode(tpe: Option[Type],
          isArray: Boolean): Int = tpe match {
    case Some(BooleanType)         =>
      if(isArray) Opcodes.BASTORE else Opcodes.ISTORE
    case Some(ByteType)            =>
      if(isArray) Opcodes.BASTORE else Opcodes.ISTORE
    case Some(ShortType)           =>
      if(isArray) Opcodes.SASTORE else Opcodes.ISTORE
    case Some(CharType)            =>
      if(isArray) Opcodes.CASTORE else Opcodes.ISTORE
    case Some(IntType)             =>
      if(isArray) Opcodes.IASTORE else Opcodes.ISTORE
    case Some(LongType)            =>
      if(isArray) Opcodes.LASTORE else Opcodes.LSTORE
    case Some(FloatType)           =>
      if(isArray) Opcodes.FASTORE else Opcodes.FSTORE
    case Some(DoubleType)          =>
      if(isArray) Opcodes.DASTORE else Opcodes.DSTORE
    case _                         =>
      if(isArray) Opcodes.AASTORE else Opcodes.ASTORE
  }

  def getField(id: IdentApi, mv: MethodVisitor): Unit = {
    val isStatic  = id.symbol.map(_.mods.isStatic).getOrElse(false)
    val className = SymbolUtils.toFullyQualifiedTypeName(
      SymbolUtils.enclosingClass(id.owner)).replaceAll("[.]", "/")
    val tpe       = id.tpe.map(tpe =>
      toInternalTypeRepresentation(tpe, true)).getOrElse("")

    if(isStatic)
      mv.visitFieldInsn(Opcodes.GETSTATIC,
        className, id.name.asString, tpe)
    else
      mv.visitFieldInsn(Opcodes.GETFIELD, className,
        id.name.asString, tpe)
  }

  def putField(id: IdentApi, mv: MethodVisitor): Unit = {
    val isStatic  = id.symbol.map(_.mods.isStatic).getOrElse(false)
    val className = SymbolUtils.toFullyQualifiedTypeName(
      SymbolUtils.enclosingClass(id.owner)).replaceAll("[.]", "/")
    val tpe       = id.tpe.map(tpe =>
      toInternalTypeRepresentation(tpe, true)).getOrElse("")

    if(isStatic)
      mv.visitFieldInsn(Opcodes.PUTSTATIC,
        className, id.name.asString, tpe)
    else
      mv.visitFieldInsn(Opcodes.PUTFIELD, className,
        id.name.asString, tpe)
  }



  def loadFromLocalVariable(tpe: Option[Type],
        index: Int,
        mv: MethodVisitor,
        isArray: Boolean): Unit = {
    val opcode = tpe match {
      case Some(BooleanType)         =>
        if(isArray) Opcodes.BALOAD else Opcodes.ILOAD
      case Some(ByteType)            =>
        if(isArray) Opcodes.BALOAD else Opcodes.ILOAD
      case Some(ShortType)           =>
        if(isArray) Opcodes.SALOAD else Opcodes.ILOAD
      case Some(CharType)            =>
        if(isArray) Opcodes.CALOAD else Opcodes.ILOAD
      case Some(IntType)             =>
        if(isArray) Opcodes.IALOAD else Opcodes.ILOAD
      case Some(LongType)            =>
        if(isArray) Opcodes.LALOAD else Opcodes.LLOAD
      case Some(FloatType)           =>
        if(isArray) Opcodes.FALOAD else Opcodes.FLOAD
      case Some(DoubleType)          =>
        if(isArray) Opcodes.DALOAD else Opcodes.DLOAD
      case _                         =>
        if(isArray) Opcodes.AALOAD else Opcodes.ALOAD
    }
    if(isArray)
      mv.visitInsn(opcode)
    else
      mv.visitVarInsn(opcode, index)
  }

  def isDoubleWord(tpe: Option[Type]): Boolean =
    tpe.map(tpe => tpe =:= LongType || tpe =:= DoubleType).getOrElse(false)

  def popIfNeeded(tree: Tree, mv: MethodVisitor): Unit = {
    if(TreeUtils.isValidStatementExpression(tree)) {
      tree.tpe match {
        case Some(LongType)            =>
          mv.visitInsn(Opcodes.POP2)
        case Some(DoubleType)          =>
          mv.visitInsn(Opcodes.POP2)
        case Some(VoidType)            =>
          ()
        case _                         =>
          mv.visitInsn(Opcodes.POP)
      }
    }
  }
}

object CodegenUtils extends CodegenUtils
