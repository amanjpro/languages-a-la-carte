/*
 * Copyright (c) <2015-2016>, see CONTRIBUTERS
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
import sana.tiny
import sana.ooj
import sana.dynj
import sana.primj
import sana.calcj
import sana.arrooj
import sana.modulej



import tiny.dsl._
import tiny.core.TransformationComponent
import tiny.names.Name
import tiny.modifiers.Flags
import guod.modifiers.ModifiersUtils
import guod.ast.TreeFactories
import guod.ast.TreeExtractors._
import guod.ast._
import modulej.ast.TreeUtils
import ooj.types.RefType
import ooj.symbols.{ClassSymbol, PackageSymbol}
import ooj.names.StdNames.CONSTRUCTOR_NAME
import primj.types.MethodType
import tiny.types.Type
import arrooj.types.ArrayType
import calcj.types._
import calcj.ast.operators._
import dynj.ast.operators._
import tiny.symbols.Symbol
import tiny.names.StdNames.noname
import guod.symbols.SymbolUtils
import guod.ast.Implicits._
import ooj.modifiers.Ops._


import org.objectweb.asm.{ClassWriter, MethodVisitor, Label}
import org.objectweb.asm.Opcodes._
import java.io.{FileOutputStream, BufferedOutputStream, File}

// Ident: DONE
// TypeUse: DONE
// Cast: DONE
// Literal: DONE
// Unary: DONE
// Binary: DONE
// Block: DONE
// While: DONE
// If: DONE
// For: DONE
// Assign: DONE
// Apply: DONE
// ValDef: DONE
// Ternary: DONE
// Return: DONE
// Label: DONE
// Break: DONE
// Continue: DONE
// Case: DONE
// Switch: DONE
// ArrayInitializer: DONE
// ArrayTypeUse: DONE
// ArrayAccess: DONE
// ArrayCreation: DONE
// Program: DONE
// PackageDef: DONE
// ClassDef: DONE
// This: DONE
// Super: DONE
// Template: DONE
// New: DONE
// Select: DONE
// Try: DONE
// MethodDef: DONE
// Catch: DONE
// Throw: DONE
// Synchronized: DONE
// CompilationUnit: DONE

case class ByteCodeWriter(destination: String,
          classWriter: Option[ClassWriter] = None,
          methodVisitor: Option[MethodVisitor] = None,
          breakTarget: Option[Label] = None,
          continueTarget: Option[Label] = None,
          labels: Map[Name, Label] = Map.empty,
          isRhs: Boolean = false)

trait CodeGenComponent extends
  TransformationComponent[(Tree, ByteCodeWriter), Unit] {
  def codegen: ((Tree, ByteCodeWriter)) => Unit
}
@component(tree, bw)
trait ProgramCodeGenComponent extends CodeGenComponent {
  (program: ProgramApi)      => {
    program.members.foreach(m => codegen((m, bw)))
  }
}

@component(tree, bw)
trait PackageDefCodeGenComponent extends CodeGenComponent {
  (pkg: PackageDefApi)      => {
    pkg.members.foreach(m => codegen((m, bw)))
  }
}


@component(tree, bw)
trait CompilationUnitCodeGenComponent extends CodeGenComponent {
  (cunit: CompilationUnitApi)      => {
    codegen((cunit.module, bw))
  }
}

@component(tree, bw)
trait ClassDefCodeGenComponent extends CodeGenComponent {
  (clazz: ClassDefApi) => {
    val cw   = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    val bw2 = bw.copy(classWriter = Some(cw))
    val mods = {
      val m = clazz.symbol.map(_.mods).getOrElse(clazz.mods)
      modifiersToBytecode(m) + ACC_SUPER
    }
    val fullName = {
      val fname = toFullyQualifiedTypeName(clazz.symbol)
      fullNameToInternalName(fname)
    }
    val (parent, interfaces) = {
      val (ps, is) = clazz.parents.partition {p =>
          !(p.symbol.map(_.mods.isInterface).getOrElse(false))
        }
      val parent = ps match {
        case _      if clazz.symbol == Some(objectClassSymbol)   =>
          None
        case p::Nil                                              =>
          Some(p)
        case p1::p2::Nil                                         =>
          if(p1.symbol == Some(objectClassSymbol))       Some(p2)
          else                                           Some(p1)
        case _                                                   =>
          Some(TreeFactories.mkTypeUse(noname, clazz.pos))
      }
      (parent, is)
    }
    val parentName = parent.map(p =>
        fullNameToInternalName(toQualifiedString(p))).getOrElse(null)
    val interfaceNames =
      interfaces.map(use =>
          fullNameToInternalName(toQualifiedString(use))).toArray
    cw.visit(targetVersion, mods,
      fullName, null, parentName,
      interfaceNames)
    codegen((clazz.body, bw2))
    cw.visitEnd()


    // write the file
    val filename  =
      if(bw.destination.size > 0)
        s"${bw.destination}${File.separator}$fullName.class"
      else s"$fullName.class"
    val path = filename.split("[./]").dropRight(2).mkString(File.separator)
    val dir  = new File(path)
    dir.mkdirs
    val byteArray = cw.toByteArray
    val bos = new BufferedOutputStream(new FileOutputStream(filename))
    Stream.continually(bos.write(byteArray))
    bos.close()
  }

  protected def objectClassSymbol: Symbol =
    SymbolUtils.objectClassSymbol
  protected def fullNameToInternalName(fullName: String): String =
    CodegenUtils.fullNameToInternalName(fullName)
  protected def targetVersion: Int =
    CodegenUtils.targetVersion
  protected def modifiersToBytecode(flags: Flags): Int = {
    ModifiersUtils.modifiersToBytecode(flags)
  }
  protected def toQualifiedString(use: UseTree): String =
    TreeUtils.toQualifiedString(use)
  protected def toFullyQualifiedTypeName(symbol: Option[Symbol]): String =
    SymbolUtils.toFullyQualifiedTypeName(symbol)
}


@component(tree, bw)
trait TemplateCodeGenComponent extends CodeGenComponent {
  (template: TemplateApi)      => {
    val (fields, methods) =
      template.members.partition(_.isInstanceOf[ValDefApi])
    fields.foreach(f => codegen((f, bw)))
    methods.foreach(m => codegen((m, bw)))
  }
}


@component(tree, bw)
trait ValDefCodeGenComponent extends CodeGenComponent {
  (valdef: ValDefApi) => {
    if(valdef.mods.isField) {
      val mods = {
        val m = valdef.symbol.map(_.mods).getOrElse(valdef.mods)
        modifiersToBytecode(m)
      }
      val name = valdef.name.asString
      val tpt  = useTreeToInternalType(valdef.tpt)
      val rhs  = {
        valdef.rhs match {
          case Literal(v: ByteConstant)     if valdef.mods.isStatic =>
            v.value.toInt
          case Literal(v: CharConstant)     if valdef.mods.isStatic =>
            v.value.toInt
          case Literal(v: ShortConstant)    if valdef.mods.isStatic =>
            v.value.toInt
          case Literal(v: IntConstant)      if valdef.mods.isStatic =>
            v.value.toInt
          case Literal(v: LongConstant)     if valdef.mods.isStatic =>
            v.value.toLong
          case Literal(v: FloatConstant)    if valdef.mods.isStatic =>
            v.value.toFloat
          case Literal(v: DoubleConstant)   if valdef.mods.isStatic =>
            v.value.toDouble
          case Literal(v: StringConstant)   if valdef.mods.isStatic =>
            v.value.toString
          case _                                                    =>
            null
        }
      }
      val cw = bw.classWriter
      cw.foreach { cw =>
        cw.visitField(mods, name, tpt, null, rhs).visitEnd
      }
    } else if(valdef.mods.isLocalVariable){
      val mv = bw.methodVisitor
      valdef.variableIndex.foreach { index =>
        if(valdef.rhs != NoTree) {
          codegen((valdef.rhs, bw))
          mv.foreach(mv => storeToLocalVariable(valdef.tpe, index,
              mv))
        }
      }
    }
  }

  protected def storeToLocalVariable(tpe: Option[Type], index: Int,
        mv: MethodVisitor): Unit =
        CodegenUtils.storeToLocalVariable(tpe, index, mv, false)

  protected def useTreeToInternalType(use: UseTree): String =
    CodegenUtils.useTreeToInternalType(use, true)

  protected def modifiersToBytecode(flags: Flags): Int = {
    ModifiersUtils.modifiersToBytecode(flags)
  }
}


@component(tree, bw)
trait MethodDefCodeGenComponent extends CodeGenComponent {
  (mthd: MethodDefApi) => {
    val mods          = {
      val m = mthd.symbol.map(_.mods).getOrElse(mthd.mods)
      modifiersToBytecode(m)
    }
    val name          = mthd.name.asString
    val ret           = {
      val name = useTreeToInternalType(mthd.ret, true)
      name
    }
    val params        = mthd.params.map(p => useTreeToInternalType(p.tpt, true))
    val desc          = s"(${params.mkString("")})$ret"
    val exceptions    = mthd.throwsClause.map(t => useTreeToInternalType(t, false))
    bw.classWriter.foreach { cw =>
      val mv = cw.visitMethod(mods, name, desc, null, exceptions.toArray)
      val bw2           = bw.copy(methodVisitor = Some(mv))
      mv.visitCode
      codegen((mthd.body, bw2))
      if(ret == "V") mv.visitInsn(RETURN)
      val maxLocals = mthd.locals
      mv.visitMaxs(0, 0)
      mv.visitEnd
    }
  }


  protected def useTreeToInternalType(use: UseTree,
          addSemicolon: Boolean): String =
    CodegenUtils.useTreeToInternalType(use, addSemicolon)

  protected def modifiersToBytecode(flags: Flags): Int = {
    ModifiersUtils.modifiersToBytecode(flags)
  }
}

@component(tree, bw)
trait SuperCodeGenComponent extends CodeGenComponent {
  (spr: SuperApi)  => {
    bw.methodVisitor.foreach(_.visitVarInsn(ALOAD, 0))
  }
}

@component(tree, bw)
trait ThisCodeGenComponent extends CodeGenComponent {
  (ths: ThisApi)  => {
    bw.methodVisitor.foreach(_.visitVarInsn(ALOAD, 0))
  }
}


@component(tree, bw)
trait ReturnCodeGenComponent extends CodeGenComponent {
  (ret: ReturnApi) => {
    val mv = bw.methodVisitor
    ret.expr.foreach(e => codegen((e, bw)))
    ret.expr.flatMap(_.tpe) match {
      case None                               =>
        mv.foreach(mv => mv.visitInsn(RETURN))
      case Some(tpe)  if tpe <:< BooleanType  =>
        mv.foreach(mv => mv.visitInsn(IRETURN))
      case Some(tpe)  if tpe <:< IntType      =>
        mv.foreach(mv => mv.visitInsn(IRETURN))
      case Some(tpe)  if tpe <:< LongType     =>
        mv.foreach(mv => mv.visitInsn(LRETURN))
      case Some(tpe)  if tpe <:< FloatType    =>
        mv.foreach(mv => mv.visitInsn(FRETURN))
      case Some(tpe)  if tpe <:< DoubleType   =>
        mv.foreach(mv => mv.visitInsn(DRETURN))
      case Some(tpe)                          =>
        mv.foreach(mv => mv.visitInsn(ARETURN))
    }
  }
}

@component(tree, bw)
trait CastCodeGenComponent extends CodeGenComponent {
  (cast: CastApi)      => {
    val mv = bw.methodVisitor
    codegen((cast.expr, bw))
    for {
      tpe1 <- cast.tpt.tpe
      tpe2 <- cast.expr.tpe
    } {
      if(tpe1 =:= LongType && tpe2 <:< IntType) {
        mv.foreach(mv => mv.visitInsn(I2L))
      } else if(tpe1 =:= FloatType && tpe2 <:< IntType) {
        mv.foreach(mv => mv.visitInsn(I2F))
      } else if(tpe1 =:= DoubleType && tpe2 <:< IntType) {
        mv.foreach(mv => mv.visitInsn(I2D))
      } else if(tpe1 <:< IntType && tpe2 =:= LongType) {
        mv.foreach(mv => mv.visitInsn(L2I))
      } else if(tpe1 =:= FloatType && tpe2 =:= LongType) {
        mv.foreach(mv => mv.visitInsn(L2F))
      } else if(tpe1 =:= DoubleType && tpe2 =:= LongType) {
        mv.foreach(mv => mv.visitInsn(L2D))
      } else if(tpe1 <:< IntType && tpe2 =:= FloatType) {
        mv.foreach(mv => mv.visitInsn(F2I))
      } else if(tpe1 =:= FloatType && tpe2 =:= FloatType) {
        mv.foreach(mv => mv.visitInsn(F2L))
      } else if(tpe1 =:= DoubleType && tpe2 =:= FloatType) {
        mv.foreach(mv => mv.visitInsn(F2D))
      } else if(tpe1 <:< IntType && tpe2 =:= DoubleType) {
        mv.foreach(mv => mv.visitInsn(D2I))
      } else if(tpe1 =:= LongType && tpe2 =:= DoubleType) {
        mv.foreach(mv => mv.visitInsn(D2L))
      } else if(tpe1 =:= FloatType && tpe2 =:= DoubleType) {
        mv.foreach(mv => mv.visitInsn(D2F))
      } else if(tpe1.isInstanceOf[RefType] && tpe2.isInstanceOf[RefType]) {
        val name = useTreeToInternalType(cast.tpt)
        mv.foreach(mv => mv.visitTypeInsn(CHECKCAST, name))
      }
    }
  }

  protected def duplicateCode(lhs: Expr): Int =
    CodegenUtils.duplicateCode(lhs)

  protected def useTreeToInternalType(use: UseTree): String =
    CodegenUtils.useTreeToInternalType(use, false)
}

@component(tree, bw)
trait BinaryCodeGenComponent extends CodeGenComponent {
  (bin: BinaryApi) => {
    (bin.tpe, bin.op) match {
      case (Some(tpe), And)                                    =>
        val mv   = bw.methodVisitor
        val end  = new Label
        val fls  = new Label
        if(!bin.isCompoundBinary)
          codegen((bin.lhs, bw))
        mv.foreach(mv => mv.visitJumpInsn(IFEQ, fls))
        codegen((bin.rhs, bw))
        mv.foreach(mv => mv.visitJumpInsn(IFEQ, fls))
        mv.foreach(mv => mv.visitInsn(ICONST_1))
        mv.foreach(mv => mv.visitJumpInsn(GOTO, end))
        mv.foreach(mv => mv.visitLabel(fls))
        mv.foreach(mv => mv.visitInsn(ICONST_0))
        mv.foreach(mv => mv.visitLabel(end))
      case (Some(tpe), Or)                                     =>
        val mv  = bw.methodVisitor
        val end = new Label
        val tru = new Label
        codegen((bin.lhs, bw))
        mv.foreach(mv => mv.visitJumpInsn(IFNE, tru))
        if(!bin.isCompoundBinary)
          codegen((bin.lhs, bw))
        mv.foreach(mv => mv.visitJumpInsn(IFNE, tru))
        mv.foreach(mv => mv.visitInsn(ICONST_0))
        mv.foreach(mv => mv.visitJumpInsn(GOTO, end))
        mv.foreach(mv => mv.visitLabel(tru))
        mv.foreach(mv => mv.visitInsn(ICONST_1))
        mv.foreach(mv => mv.visitLabel(end))
      case (Some(tpe), InstanceOf)                            =>
        bin.rhs match {
          case use: UseTree          =>
            val mv = bw.methodVisitor
            codegen((bin.lhs, bw))
            val name = useTreeToInternalType(use)
            mv.foreach(mv => mv.visitTypeInsn(INSTANCEOF, name))
          case _                     =>
        }
      case _                                                  =>
        codegenArith(bin, bw)
    }
  }

  protected def duplicateCode(lhs: Expr): Int =
    CodegenUtils.duplicateCode(lhs)

  protected def useTreeToInternalType(use: UseTree): String =
    CodegenUtils.useTreeToInternalType(use, false)

  def codegenArith(bin: BinaryApi, bw: ByteCodeWriter): Unit = {
    val mv = bw.methodVisitor
    if(!bin.isCompoundBinary)
      codegen((bin.lhs, bw))
    codegen((bin.rhs, bw))
    (bin.tpe, bin.op) match {
      case (Some(tpe), Add) if tpe <:< IntType           =>
        mv.foreach(mv => mv.visitInsn(IADD))
      case (Some(tpe), Add) if tpe <:< LongType          =>
        mv.foreach(mv => mv.visitInsn(LADD))
      case (Some(tpe), Add) if tpe <:< FloatType         =>
        mv.foreach(mv => mv.visitInsn(FADD))
      case (Some(tpe), Add) if tpe <:< DoubleType        =>
        mv.foreach(mv => mv.visitInsn(DADD))
      case (Some(tpe), Add)                              =>
        mv.foreach( mv =>
          mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/String",
            "concat", "(Ljava/lang/String;)Ljava/lang/String;", false))
      case (Some(tpe), Sub) if tpe <:< IntType           =>
        mv.foreach(mv => mv.visitInsn(ISUB))
      case (Some(tpe), Sub) if tpe <:< LongType          =>
        mv.foreach(mv => mv.visitInsn(LSUB))
      case (Some(tpe), Sub) if tpe <:< FloatType         =>
        mv.foreach(mv => mv.visitInsn(FSUB))
      case (Some(tpe), Sub) if tpe <:< DoubleType        =>
        mv.foreach(mv => mv.visitInsn(DSUB))
      case (Some(tpe), Mul) if tpe <:< IntType           =>
        mv.foreach(mv => mv.visitInsn(IMUL))
      case (Some(tpe), Mul) if tpe <:< LongType          =>
        mv.foreach(mv => mv.visitInsn(LMUL))
      case (Some(tpe), Mul) if tpe <:< FloatType         =>
        mv.foreach(mv => mv.visitInsn(FMUL))
      case (Some(tpe), Mul) if tpe <:< DoubleType        =>
        mv.foreach(mv => mv.visitInsn(DMUL))
      case (Some(tpe), Div) if tpe <:< IntType           =>
        mv.foreach(mv => mv.visitInsn(IDIV))
      case (Some(tpe), Div) if tpe <:< LongType          =>
        mv.foreach(mv => mv.visitInsn(LDIV))
      case (Some(tpe), Div) if tpe <:< FloatType         =>
        mv.foreach(mv => mv.visitInsn(FDIV))
      case (Some(tpe), Div) if tpe <:< DoubleType        =>
        mv.foreach(mv => mv.visitInsn(DDIV))
      case (Some(tpe), Mod) if tpe <:< IntType           =>
        mv.foreach(mv => mv.visitInsn(IREM))
      case (Some(tpe), Mod) if tpe <:< LongType          =>
        mv.foreach(mv => mv.visitInsn(LREM))
      case (Some(tpe), Mod) if tpe <:< FloatType         =>
        mv.foreach(mv => mv.visitInsn(FREM))
      case (Some(tpe), Mod) if tpe <:< DoubleType        =>
        mv.foreach(mv => mv.visitInsn(DREM))
      case (Some(tpe), BAnd) if tpe <:< IntType          =>
        mv.foreach(mv => mv.visitInsn(IAND))
      case (Some(tpe), BAnd) if tpe <:< LongType         =>
        mv.foreach(mv => mv.visitInsn(LAND))
      case (Some(tpe), BOr) if tpe <:< IntType           =>
        mv.foreach(mv => mv.visitInsn(IOR))
      case (Some(tpe), BOr) if tpe <:< LongType          =>
        mv.foreach(mv => mv.visitInsn(LOR))
      case (Some(tpe), BXor) if tpe <:< IntType          =>
        mv.foreach(mv => mv.visitInsn(LXOR))
      case (Some(tpe), BXor) if tpe <:< LongType         =>
        mv.foreach(mv => mv.visitInsn(LXOR))
      case (Some(tpe), SHL) if tpe <:< IntType           =>
        mv.foreach(mv => mv.visitInsn(ISHL))
      case (Some(tpe), SHL) if tpe <:< LongType          =>
        mv.foreach(mv => mv.visitInsn(LSHL))
      case (Some(tpe), SHR) if tpe <:< IntType           =>
        mv.foreach(mv => mv.visitInsn(ISHR))
      case (Some(tpe), SHR) if tpe <:< LongType          =>
        mv.foreach(mv => mv.visitInsn(LSHR))
      case (Some(tpe), USHR) if tpe <:< IntType          =>
        mv.foreach(mv => mv.visitInsn(IUSHR))
      case (Some(tpe), USHR) if tpe <:< LongType         =>
        mv.foreach(mv => mv.visitInsn(LUSHR))
      case (Some(_), Eq)                                 =>
        bin.lhs.tpe.foreach {tpe =>
          val l0 = new Label
          if(tpe <:< IntType || tpe <:< BooleanType) {
            mv.foreach(mv => mv.visitJumpInsn(IF_ICMPNE, l0))
          } else if(tpe <:< LongType) {
            mv.foreach(mv => mv.visitInsn(LCMP))
            mv.foreach(mv => mv.visitJumpInsn(IFNE, l0))
          } else if(tpe <:< FloatType) {
            mv.foreach(mv => mv.visitInsn(FCMPL))
            mv.foreach(mv => mv.visitJumpInsn(IFNE, l0))
          } else if(tpe <:< DoubleType) {
            mv.foreach(mv => mv.visitInsn(DCMPL))
            mv.foreach(mv => mv.visitJumpInsn(IFNE, l0))
          } else {
            mv.foreach(mv => mv.visitJumpInsn(IF_ACMPNE, l0))
          }
          mv.foreach(mv => mv.visitInsn(ICONST_1))
          val l1 = new Label
          mv.foreach(mv => mv.visitJumpInsn(GOTO, l1))
          mv.foreach(mv => mv.visitLabel(l0))
          mv.foreach(mv => mv.visitInsn(ICONST_0))
          mv.foreach(mv => mv.visitLabel(l1))
        }
      case (Some(_), Neq)                                =>
        bin.lhs.tpe.foreach {tpe =>
          val l0 = new Label
          if(tpe <:< IntType || tpe <:< BooleanType) {
            mv.foreach(mv => mv.visitJumpInsn(IF_ICMPEQ, l0))
          }
          else if(tpe <:< LongType) {
            mv.foreach(mv => mv.visitInsn(LCMP))
            mv.foreach(mv => mv.visitJumpInsn(IFEQ, l0))
          }
          else if(tpe <:< FloatType) {
            mv.foreach(mv => mv.visitInsn(FCMPL))
            mv.foreach(mv => mv.visitJumpInsn(IFEQ, l0))
          }
          else if(tpe <:< DoubleType) {
            mv.foreach(mv => mv.visitInsn(DCMPL))
            mv.foreach(mv => mv.visitJumpInsn(IFEQ, l0))
          }
          else {
            mv.foreach(mv => mv.visitJumpInsn(IF_ACMPEQ, l0))
          }
          mv.foreach(mv => mv.visitInsn(ICONST_1))
          val l1 = new Label
          mv.foreach(mv => mv.visitJumpInsn(GOTO, l1))
          mv.foreach(mv => mv.visitLabel(l0))
          mv.foreach(mv => mv.visitInsn(ICONST_0))
          mv.foreach(mv => mv.visitLabel(l1))
        }
      case (Some(_), Gt)                                 =>
        bin.lhs.tpe.foreach {tpe =>
          val l0 = new Label
          if(tpe <:< IntType) {
            mv.foreach(mv => mv.visitJumpInsn(IF_ICMPLE, l0))
          }
          else if(tpe <:< LongType) {
            mv.foreach(mv => mv.visitInsn(LCMP))
            mv.foreach(mv => mv.visitJumpInsn(IFLE, l0))
          }
          else if(tpe <:< FloatType) {
            mv.foreach(mv => mv.visitInsn(FCMPG))
            mv.foreach(mv => mv.visitJumpInsn(IFLE, l0))
          }
          else if(tpe <:< DoubleType) {
            mv.foreach(mv => mv.visitInsn(DCMPG))
            mv.foreach(mv => mv.visitJumpInsn(IFLE, l0))
          }
          mv.foreach(mv => mv.visitInsn(ICONST_1))
          val l1 = new Label
          mv.foreach(mv => mv.visitJumpInsn(GOTO, l1))
          mv.foreach(mv => mv.visitLabel(l0))
          mv.foreach(mv => mv.visitInsn(ICONST_0))
          mv.foreach(mv => mv.visitLabel(l1))
        }
      case (Some(_), Lt)                                 =>
        bin.lhs.tpe.foreach {tpe =>
          val l0 = new Label
          if(tpe <:< IntType) {
            mv.foreach(mv => mv.visitJumpInsn(IF_ICMPGE, l0))
          }
          else if(tpe <:< LongType) {
            mv.foreach(mv => mv.visitInsn(LCMP))
            mv.foreach(mv => mv.visitJumpInsn(IFGE, l0))
          }
          else if(tpe <:< FloatType) {
            mv.foreach(mv => mv.visitInsn(FCMPG))
            mv.foreach(mv => mv.visitJumpInsn(IFGE, l0))
          }
          else if(tpe <:< DoubleType) {
            mv.foreach(mv => mv.visitInsn(DCMPG))
            mv.foreach(mv => mv.visitJumpInsn(IFGE, l0))
          }
          mv.foreach(mv => mv.visitInsn(ICONST_1))
          val l1 = new Label
          mv.foreach(mv => mv.visitJumpInsn(GOTO, l1))
          mv.foreach(mv => mv.visitLabel(l0))
          mv.foreach(mv => mv.visitInsn(ICONST_0))
          mv.foreach(mv => mv.visitLabel(l1))
        }
      case (Some(tpe), Le)                               =>
        bin.lhs.tpe.foreach {tpe =>
          val l0 = new Label
          if(tpe <:< IntType) {
            mv.foreach(mv => mv.visitJumpInsn(IF_ICMPGT, l0))
          }
          else if(tpe <:< LongType) {
            mv.foreach(mv => mv.visitInsn(LCMP))
            mv.foreach(mv => mv.visitJumpInsn(IFGT, l0))
          }
          else if(tpe <:< FloatType) {
            mv.foreach(mv => mv.visitInsn(FCMPG))
            mv.foreach(mv => mv.visitJumpInsn(IFGT, l0))
          }
          else if(tpe <:< DoubleType) {
            mv.foreach(mv => mv.visitInsn(DCMPG))
            mv.foreach(mv => mv.visitJumpInsn(IFGT, l0))
          }
          mv.foreach(mv => mv.visitInsn(ICONST_1))
          val l1 = new Label
          mv.foreach(mv => mv.visitJumpInsn(GOTO, l1))
          mv.foreach(mv => mv.visitLabel(l0))
          mv.foreach(mv => mv.visitInsn(ICONST_0))
          mv.foreach(mv => mv.visitLabel(l1))
        }
      case (Some(tpe), Ge)                               =>
        bin.lhs.tpe.foreach {tpe =>
          val l0 = new Label
          if(tpe <:< IntType) {
            mv.foreach(mv => mv.visitJumpInsn(IF_ICMPLT, l0))
          }
          else if(tpe <:< LongType) {
            mv.foreach(mv => mv.visitInsn(LCMP))
            mv.foreach(mv => mv.visitJumpInsn(IFLT, l0))
          }
          else if(tpe <:< FloatType) {
            mv.foreach(mv => mv.visitInsn(FCMPG))
            mv.foreach(mv => mv.visitJumpInsn(IFLT, l0))
          }
          else if(tpe <:< DoubleType) {
            mv.foreach(mv => mv.visitInsn(DCMPG))
            mv.foreach(mv => mv.visitJumpInsn(IFLT, l0))
          }
          mv.foreach(mv => mv.visitInsn(ICONST_1))
          val l1 = new Label
          mv.foreach(mv => mv.visitJumpInsn(GOTO, l1))
          mv.foreach(mv => mv.visitLabel(l0))
          mv.foreach(mv => mv.visitInsn(ICONST_0))
          mv.foreach(mv => mv.visitLabel(l1))
        }
      case _                                             =>
        ()
    }
  }
}


@component(tree, bw)
trait UnaryCodeGenComponent extends CodeGenComponent {
  (unary: UnaryApi) => {
    val mv = bw.methodVisitor
    (unary.tpe, unary.op) match {
      case (Some(tpe), Not)                                                    =>
        codegen((unary.expr, bw))
        val l0 = new Label
        mv.foreach(mv => mv.visitJumpInsn(IFNE, l0))
        mv.foreach(mv => mv.visitInsn(ICONST_1))
        val l1 = new Label
        mv.foreach(mv => mv.visitJumpInsn(GOTO, l1))
        mv.foreach(mv => mv.visitLabel(l0))
        mv.foreach(mv => mv.visitInsn(ICONST_0))
        mv.foreach(mv => mv.visitLabel(l1))
      case (Some(tpe), BCompl)                                                 =>
        codegen((unary.expr, bw))
        if(tpe <:< IntType) {
          mv.foreach(mv => mv.visitInsn(ICONST_M1))
          mv.foreach(mv =>  mv.visitInsn(IXOR))
        } else {
          mv.foreach(mv => mv.visitLdcInsn(-1L))
          mv.foreach(mv =>  mv.visitInsn(LXOR))
        }
      case (Some(tpe), uop)   if uop == Inc || uop == Dec                      =>
        val op       = translateOp(unary.expr.tpe, unary.op)
        val lit      = one(unary.tpe)
        val dupOp    = unary.expr match {
          case ArrayAccess(a, i)                                        =>
            codegen((a, bw))
            codegen((i, bw))
            mv.foreach(mv => mv.visitInsn(DUP2))
            mv.foreach(mv =>
                loadFromLocalVariable(unary.expr.tpe, -1, mv, true))
            if(isDoubleWord(unary.expr.tpe))
              DUP2_X2
            else
              DUP_X2
          case Select(q, t: IdentApi)            if !isStatic(t.symbol) =>
            codegen((q, bw))
            mv.foreach(mv => mv.visitInsn(DUP))
            mv.foreach(mv => getField(t, mv))
            if(isDoubleWord(unary.expr.tpe))
              DUP2_X1
            else
              DUP_X1
          case expr                    =>
            codegen((expr, bw))
            if(isDoubleWord(unary.expr.tpe))
              DUP2
            else
              DUP
        }

        if(!unary.isPostfix) {
          codegen((lit, bw))
          mv.foreach(mv => mv.visitInsn(op))
        }
        // store the number at the top of the stack
        bw.methodVisitor.foreach(mv => mv.visitInsn(dupOp))

        if(unary.isPostfix) {
          codegen((lit, bw))
          mv.foreach(mv => mv.visitInsn(op))
        }

        unary.expr match {
          case _: ArrayAccessApi       =>
            mv.foreach(mv => storeToLocalVariable(unary.expr.tpe, mv))
          case Select(_, t: IdentApi)  =>
            mv.foreach(mv => putField(t, mv))
          case expr                    =>
            codegen((expr, bw.copy(isRhs = true)))
        }
      case (Some(tpe), Neg) if tpe <:< IntType                                 =>
        codegen((unary.expr, bw))
        mv.foreach(mv => mv.visitInsn(INEG))
      case (Some(tpe), Neg) if tpe <:< LongType                                =>
        codegen((unary.expr, bw))
        mv.foreach(mv => mv.visitInsn(LNEG))
      case (Some(tpe), Neg) if tpe <:< FloatType                               =>
        codegen((unary.expr, bw))
        mv.foreach(mv => mv.visitInsn(FNEG))
      case (Some(tpe), Neg) if tpe <:< DoubleType                              =>
        codegen((unary.expr, bw))
        mv.foreach(mv => mv.visitInsn(DNEG))
      case (Some(tpe), Pos)                                                    =>
        ()
      case _                                                                   =>
        ()
    }
  }

  protected def isStatic(symbol: Option[Symbol]): Boolean =
    symbol.map(_.mods.isStatic).getOrElse(false)

  protected def isDoubleWord(tpe: Option[Type]): Boolean =
    CodegenUtils.isDoubleWord(tpe)

  protected def getField(id: IdentApi, mv: MethodVisitor): Unit =
    CodegenUtils.getField(id, mv)

  protected def putField(id: IdentApi, mv: MethodVisitor): Unit =
    CodegenUtils.putField(id, mv)

  protected def loadFromLocalVariable(tpe: Option[Type],
        index: Int,
        mv: MethodVisitor,
        isArray: Boolean): Unit =
    CodegenUtils.loadFromLocalVariable(tpe, index, mv, isArray)


  protected def translateOp(tpe: Option[Type], uop: Op): Int = {
    val res = tpe.map { tpe =>
      tpe match {
        case LongType            if uop == Inc   => LADD
        case LongType                            => LSUB
        case FloatType           if uop == Inc   => FADD
        case FloatType                           => FSUB
        case DoubleType          if uop == Inc   => DADD
        case DoubleType                          => DSUB
        case _                   if uop == Inc   => IADD
        case _                                   => ISUB
      }
    }
    res.getOrElse(IADD)
  }


  protected def duplicateCode(expr: Expr): Int = expr match {
    case _: ArrayAccessApi      => DUP2
    case _                      => CodegenUtils.duplicateCode(expr)
  }

  protected def storeToLocalVariable(tpe: Option[Type],
        mv: MethodVisitor): Unit =
        CodegenUtils.storeToLocalVariable(tpe, -1, mv, true)

  protected def enclosingClass(owner: Option[Symbol]): Option[Symbol] =
    SymbolUtils.enclosingClass(owner)

  protected def toFullyQualifiedTypeName(symbol: Option[Symbol]): String =
    SymbolUtils.toFullyQualifiedTypeName(symbol)

  protected def toInternalTypeRepresentation(tpe: Type): String =
    CodegenUtils.toInternalTypeRepresentation(tpe, true)


  protected def one(tpe: Option[Type]): Expr = tpe match {
    case Some(tpe)    if tpe <:< IntType        =>
      TreeFactories.mkLiteral(IntConstant(1))
    case Some(tpe)    if tpe <:< LongType       =>
      TreeFactories.mkLiteral(LongConstant(1l))
    case Some(tpe)    if tpe <:< FloatType      =>
      TreeFactories.mkLiteral(FloatConstant(1f))
    case Some(tpe)    if tpe <:< DoubleType     =>
      TreeFactories.mkLiteral(DoubleConstant(1))

  }
}


@component(tree, bw)
trait IfCodeGenComponent extends CodeGenComponent {
  (ifelse: IfApi) => {
    codegen((ifelse.cond, bw))
    val mv = bw.methodVisitor
    val l0 = new Label
    mv.foreach(mv => mv.visitJumpInsn(IFNE, l0))
    codegen((ifelse.thenp, bw))
    val l1 = new Label
    mv.foreach(mv => mv.visitJumpInsn(GOTO, l1))
    mv.foreach(mv => mv.visitLabel(l0))
    codegen((ifelse.elsep, bw))
    mv.foreach(mv => mv.visitLabel(l1))
  }
}


@component(tree, bw)
trait WhileCodeGenComponent extends CodeGenComponent {
  (wile: WhileApi) => {
    val next   = new Label
    val mv = bw.methodVisitor
    val cond = new Label
    if(!wile.isDoWhile) {
      mv.foreach(mv => mv.visitJumpInsn(GOTO, cond))
    }
    val body = new Label
    mv.foreach(mv => mv.visitLabel(body))
    codegen((wile.body, bw.copy(breakTarget = Some(next),
      continueTarget = Some(cond))))
    mv.foreach(mv => mv.visitLabel(cond))
    codegen((wile.cond, bw))
    mv.foreach(mv => mv.visitJumpInsn(IFEQ, body))
    mv.foreach(mv => mv.visitLabel(next))
  }
}

@component(tree, bw)
trait TernaryCodeGenComponent extends CodeGenComponent {
  (tern: TernaryApi) => {
    codegen((tern.cond, bw))
    val mv = bw.methodVisitor
    val l0 = new Label
    mv.foreach(mv => mv.visitJumpInsn(IFNE, l0))
    codegen((tern.thenp, bw))
    val l1 = new Label
    mv.foreach(mv => mv.visitJumpInsn(GOTO, l1))
    mv.foreach(mv => mv.visitLabel(l0))
    codegen((tern.elsep, bw))
    mv.foreach(mv => mv.visitLabel(l1))
  }
}


@component(tree, bw)
trait ForCodeGenComponent extends CodeGenComponent {
  (forloop: ForApi) => {
    val mv = bw.methodVisitor
    val next   = new Label
    forloop.inits.foreach { init =>
      codegen((init, bw))
      bw.methodVisitor.foreach(mv => popIfNeeded(init, mv))
    }
    val steps = new Label
    val cond = new Label
    val body = new Label
    mv.foreach(mv => mv.visitJumpInsn(GOTO, cond))

    mv.foreach(mv => mv.visitLabel(body))
    codegen((forloop.body, bw.copy(breakTarget = Some(next),
      continueTarget = Some(steps))))

    mv.foreach(mv => mv.visitLabel(steps))
    forloop.steps.foreach { step =>
      codegen((step, bw))
      bw.methodVisitor.foreach(mv => popIfNeeded(step, mv))
    }

    mv.foreach(mv => mv.visitLabel(cond))
    codegen((forloop.cond, bw))
    mv.foreach(mv => mv.visitJumpInsn(IFNE, body))

    mv.foreach(mv => mv.visitLabel(next))
  }

  protected def popIfNeeded(tree: Tree, mv: MethodVisitor): Unit =
    CodegenUtils.popIfNeeded(tree, mv)
}


@component(tree, bw)
trait SwitchCodeGenComponent extends CodeGenComponent {
  (switch: SwitchApi)  => {
    codegen((switch.expr, bw))
    val mv = bw.methodVisitor
    val next = new Label
    val nonDefaultCases = switch.cases.filter { cse =>
      cse.guards != Nil
    }
    val labels = nonDefaultCases.flatMap { cse =>
      Some(new Label)
    }
    val defaultCase  = switch.cases.filter(_.guards == Nil)
    val defaultLabel = defaultCase match {
      case Nil                  => next
      case _                    => new Label
    }
    val (guards, allLabels) = {
      val res = nonDefaultCases.zip(labels).flatMap { it =>
        val cse = it._1
        val lbl = it._2
        cse.guards.flatMap { guard =>
          guard.asInstanceOf[LiteralApi].constant.value match {
            case i: Int         => List((i, lbl))
            case i: Char        => List((i.toInt, lbl))
            case i: Short       => List((i.toInt, lbl))
            case i: Byte        => List((i.toInt, lbl))
            case _              => Nil
          }
        }
      }
      val res2 = res.sortWith((e1, e2) => e1._1 < e2._1)
      res2.toArray.unzip
    }
    mv.foreach(mv => mv.visitLookupSwitchInsn(defaultLabel, guards, allLabels))
    nonDefaultCases.zip(labels).foreach { it =>
      val cse = it._1
      val lbl = it._2
      mv.foreach(mv => mv.visitLabel(lbl))
      codegen((cse.body, bw.copy(breakTarget = Some(next))))
    }
    defaultCase match {
      case List(cse)            =>
        mv.foreach(mv => mv.visitLabel(defaultLabel))
        codegen((cse.body, bw.copy(breakTarget = Some(next))))
      case _                    => ()
    }
    mv.foreach(mv => mv.visitLabel(next))
  }
}


@component(tree, bw)
trait BreakCodeGenComponent extends CodeGenComponent {
  (brk: BreakApi) => {
    val mv = bw.methodVisitor
    brk.label match {
      case Some(name)          =>
        bw.labels.get(name).foreach {lbl =>
          mv.foreach(mv => mv.visitJumpInsn(GOTO, lbl))
        }
      case None                =>
        bw.breakTarget.foreach {lbl =>
          mv.foreach(mv => mv.visitJumpInsn(GOTO, lbl))
        }
    }
  }
}

@component(tree, bw)
trait ContinueCodeGenComponent extends CodeGenComponent {
  (cntnue: ContinueApi) => {
    val mv = bw.methodVisitor
    cntnue.label match {
      case Some(name)          =>
        bw.labels.get(name).foreach {lbl =>
          mv.foreach(mv => mv.visitJumpInsn(GOTO, lbl))
        }
      case None                =>
        bw.continueTarget.foreach {lbl =>
          mv.foreach(mv => mv.visitJumpInsn(GOTO, lbl))
        }
    }
  }
}


@component(tree, bw)
trait LabelCodeGenComponent extends CodeGenComponent {
  (lbl: LabelApi) => {
    val l = new Label
    val bw2 = bw.copy(labels = bw.labels + (lbl.name -> l))
    codegen((lbl.stmt, bw2))
  }
}


@component(tree, bw)
trait LiteralCodeGenComponent extends CodeGenComponent {
  (lit: LiteralApi) => {
    val mv = bw.methodVisitor
    lit match {
      case Literal(BooleanConstant(true))         =>
        mv.foreach(mv => mv.visitInsn(ICONST_1))
      case Literal(BooleanConstant(false))        =>
        mv.foreach(mv => mv.visitInsn(ICONST_0))
      case Literal(ByteConstant(v))               =>
        mv.foreach(mv => mv.visitIntInsn(BIPUSH, v))
      case Literal(ShortConstant(v))              =>
        mv.foreach(mv => mv.visitIntInsn(SIPUSH, v))
      case Literal(CharConstant(v))              =>
        mv.foreach(mv => mv.visitLdcInsn(v))
      case Literal(IntConstant(v))                =>
        mv.foreach(mv => mv.visitLdcInsn(v))
      case Literal(LongConstant(v))               =>
        mv.foreach(mv => mv.visitLdcInsn(v))
      case Literal(FloatConstant(v))              =>
        mv.foreach(mv => mv.visitLdcInsn(v))
      case Literal(DoubleConstant(v))             =>
        mv.foreach(mv => mv.visitLdcInsn(v))
      case Literal(StringConstant(v))             =>
        mv.foreach(mv => mv.visitLdcInsn(v.substring(1, v.length -1)))
      case Literal(NullConstant)                  =>
        mv.foreach(mv => mv.visitInsn(ACONST_NULL))
    }
  }
}



@component(tree, bw)
trait BlockCodeGenComponent extends CodeGenComponent {
  (block: BlockApi) => {
    block.stmts.foreach { stmt =>
      codegen((stmt, bw))
      bw.methodVisitor.foreach(mv => popIfNeeded(stmt, mv))
    }
  }
  protected def popIfNeeded(tree: Tree, mv: MethodVisitor): Unit =
    CodegenUtils.popIfNeeded(tree, mv)
}


@component(tree, bw)
trait ArrayCreationCodeGenComponent extends CodeGenComponent {
  (creation: ArrayCreationApi) => {
    val mv = bw.methodVisitor
    val size = creation.size
    size.foreach(size => codegen((size, bw)))
    elementTypeToOpcode(creation.array.tpe) match {
      case Right(opcode)           =>
        mv.foreach(mv => mv.visitIntInsn(NEWARRAY, opcode))
      case Left(tpe)               =>
        mv.foreach(mv => mv.visitTypeInsn(NEWARRAY, tpe))
    }
  }

  protected def elementTypeToOpcode(etpe: Option[Type]): Either[String, Int] =
    CodegenUtils.elementTypeToOpcode(etpe)

}
@component(tree, bw)
trait ArrayInitializerCodeGenComponent extends CodeGenComponent {
  (init: ArrayInitializerApi) => {
    val mv = bw.methodVisitor
    val size = init.elements.size
    mv.foreach(mv => mv.visitLdcInsn(size))
    elementTypeToOpcode(init.componentType.map(f => f())) match {
      case Right(opcode)           =>
        mv.foreach(mv => mv.visitIntInsn(NEWARRAY, opcode))
      case Left(tpe)               =>
        mv.foreach(mv => mv.visitTypeInsn(ANEWARRAY, tpe))
    }
    val elements = init.elements.zipWithIndex
    elements.foreach { e =>
      val element = e._1
      val index   = e._2
      mv.foreach(mv => mv.visitInsn(DUP))
      mv.foreach(mv => mv.visitLdcInsn(index))
      codegen((element, bw))
      mv.foreach(mv => storeToLocalVariable(element.tpe,
            mv))
    }
  }

  protected def duplicateCode(lhs: Expr): Int = {
    CodegenUtils.duplicateCode(lhs)
  }

  protected def storeToLocalVariable(tpe: Option[Type],
        mv: MethodVisitor): Unit =
        CodegenUtils.storeToLocalVariable(tpe, -1, mv, true)

  protected def elementTypeToOpcode(etpe: Option[Type]): Either[String, Int] =
    CodegenUtils.elementTypeToOpcode(etpe)
}


@component(tree, bw)
trait ArrayAccessCodeGenComponent extends CodeGenComponent {
  (access: ArrayAccessApi)               => {
    val mv = bw.methodVisitor
    codegen((access.array, bw.copy(isRhs = false)))
    codegen((access.index, bw.copy(isRhs = false)))
    if(bw.isRhs) {
      mv.foreach(mv => storeToLocalVariable(access.tpe, mv))
    } else {
      mv.foreach(mv => loadFromLocalVariable(access.tpe, mv))
    }
  }

  protected def storeToLocalVariable(tpe: Option[Type],
        mv: MethodVisitor): Unit =
        CodegenUtils.storeToLocalVariable(tpe, -1, mv, true)

  protected def loadFromLocalVariable(tpe: Option[Type],
        mv: MethodVisitor): Unit =
        CodegenUtils.loadFromLocalVariable(tpe, -1, mv, true)
}

@component(tree, bw)
trait AssignCodeGenComponent extends CodeGenComponent {
  (assign: AssignApi) => {
    val mv = bw.methodVisitor
    assign.lhs match {
      case ArrayAccess(a, i)          if isCompoundBinary(assign.rhs) =>
        codegen((a, bw))
        codegen((i, bw))
        mv.foreach(mv => mv.visitInsn(DUP2))
        mv.foreach(mv =>
            loadFromLocalVariable(assign.lhs.tpe, -1, mv, true))
        codegen((assign.rhs, bw))
        if(isDoubleWord(assign.rhs.tpe))
          bw.methodVisitor.foreach(mv => mv.visitInsn(DUP2_X2))
        else
          bw.methodVisitor.foreach(mv => mv.visitInsn(DUP_X2))
        mv.foreach(mv => storeToLocalVariable(assign.lhs.tpe, mv))
      case ArrayAccess(a, i)                                           =>
        codegen((a, bw))
        codegen((i, bw))
        codegen((assign.rhs, bw))
        if(isDoubleWord(assign.rhs.tpe))
          bw.methodVisitor.foreach(mv => mv.visitInsn(DUP2_X2))
        else
          bw.methodVisitor.foreach(mv => mv.visitInsn(DUP_X2))
        mv.foreach(mv => storeToLocalVariable(assign.lhs.tpe, mv))
      case Select(q, t: IdentApi)  if isCompoundBinary(assign.rhs) &&
                                      !isStatic(t.symbol)              =>
        codegen((q, bw))
        mv.foreach(mv => mv.visitInsn(DUP))
        mv.foreach(mv => getField(t, mv))
        codegen((assign.rhs, bw))
        if(isDoubleWord(assign.rhs.tpe))
          bw.methodVisitor.foreach(mv => mv.visitInsn(DUP2_X1))
        else
          bw.methodVisitor.foreach(mv => mv.visitInsn(DUP_X1))
        mv.foreach(mv => putField(t, mv))
      case Select(q, t)          if !isStatic(t.symbol)                =>
        codegen((q, bw))
        codegen((assign.rhs, bw))
        if(isDoubleWord(assign.rhs.tpe))
          bw.methodVisitor.foreach(mv => mv.visitInsn(DUP2_X1))
        else
          bw.methodVisitor.foreach(mv => mv.visitInsn(DUP_X1))
        codegen((t, bw.copy(isRhs = true)))
      case lhs                                                         =>
        if(isCompoundBinary(assign.rhs)) {
          codegen((lhs, bw))
        }
        codegen((assign.rhs, bw))
        bw.methodVisitor.foreach(mv =>
          mv.visitInsn(duplicateCode(lhs)))
        codegen((lhs, bw.copy(isRhs = true)))
    }
  }

  protected def isStatic(symbol: Option[Symbol]): Boolean =
    symbol.map(_.mods.isStatic).getOrElse(false)

  protected def isDoubleWord(tpe: Option[Type]): Boolean =
    CodegenUtils.isDoubleWord(tpe)

  protected def getField(id: IdentApi, mv: MethodVisitor): Unit =
    CodegenUtils.getField(id, mv)

  protected def putField(id: IdentApi, mv: MethodVisitor): Unit =
    CodegenUtils.putField(id, mv)



  protected def isCompoundBinary(expr: Expr): Boolean = expr match {
    case Cast(_, bin: BinaryApi)  => bin.isCompoundBinary
    case bin: BinaryApi           => bin.isCompoundBinary
    case _                        => false
  }

  protected def duplicateCode(lhs: Expr): Int = {
    CodegenUtils.duplicateCode(lhs)
  }

  protected def loadFromLocalVariable(tpe: Option[Type],
        index: Int,
        mv: MethodVisitor,
        isArray: Boolean): Unit =
    CodegenUtils.loadFromLocalVariable(tpe, index, mv, isArray)

  protected def storeToLocalVariable(tpe: Option[Type],
        mv: MethodVisitor): Unit =
        CodegenUtils.storeToLocalVariable(tpe, -1, mv, true)

  protected def toFullyQualifiedTypeName(symbol: Option[Symbol]): String =
    SymbolUtils.toFullyQualifiedTypeName(symbol)
}


@component(tree, bw)
trait ApplyCodeGenComponent extends CodeGenComponent {
  (app: ApplyApi) => {
    val mv = bw.methodVisitor
    codegen((app.fun, bw))
    val (className, funName, funSig) = app.fun match {
      case Select(t, f)         =>
        val cname = toFullyQualifiedTypeName(t.symbol)
        val fname = f.name.asString
        val sig   = f.tpe match {
          case Some(MethodType(r, params))     =>
            val ptpes = params.map(toInternalTypeRepresentation(_)).mkString("")
            s"(${ptpes})${toInternalTypeRepresentation(r)}"
          case _                               =>
            ""
        }
        (cname.replaceAll("[.]", "/"), fname, sig)
      case _                    => ("", "", "")
    }
    if(app.fun.symbol.map(_.mods.isStatic).getOrElse(false)) {
      app.args.foreach(arg => codegen((arg, bw)))
      mv.foreach( mv =>
          mv.visitMethodInsn(INVOKESTATIC, className, funName, funSig, false))
    } else {
      app.args.foreach { arg =>
        codegen((arg, bw))
      }
      val opcode = if(app.fun.symbol.map(_.mods.isPrivateAcc).getOrElse(false)) {
        INVOKESPECIAL
      } else {
        app.fun match {
          case Select(_: SuperApi, _)                           => INVOKESPECIAL
          case Select(_, id) if id.name == CONSTRUCTOR_NAME     => INVOKESPECIAL
          case _                                                => {
            INVOKEVIRTUAL
          }
        }
      }
      mv.foreach( mv =>
          mv.visitMethodInsn(opcode, className, funName, funSig, false))
    }
  }

  protected def toFullyQualifiedTypeName(symbol: Option[Symbol]): String =
    SymbolUtils.toFullyQualifiedTypeName(symbol)

  protected def toInternalTypeRepresentation(tpe: Type): String =
    CodegenUtils.toInternalTypeRepresentation(tpe, true)
}

@component(tree, bw)
trait NewCodeGenComponent extends CodeGenComponent {
  (nw: NewApi) => {
    val mv = bw.methodVisitor
    val cname = nw.tpe.map(toInternalTypeRepresentation(_)).getOrElse("")
    mv.foreach(mv => mv.visitTypeInsn(NEW, cname))
    mv.foreach(mv => mv.visitInsn(DUP))
    codegen((nw.app, bw))
  }

  protected def toInternalTypeRepresentation(tpe: Type): String =
    CodegenUtils.toInternalTypeRepresentation(tpe, false)
}



@component(tree, bw)
trait ThrowCodeGenComponent extends CodeGenComponent {
  (thrw: ThrowApi) => {
    val mv = bw.methodVisitor
    codegen((thrw.expr, bw))
    mv.foreach(mv => mv.visitInsn(ATHROW))
  }
}


@component(tree, bw)
trait SynchronizedCodeGenComponent extends CodeGenComponent {
  (synch: SynchronizedApi) => {
    val mv = bw.methodVisitor
    synch.identifierIndices.foreach { indices =>
      val index1 = indices._1
      val index2 = indices._2
      val l0 = new Label
      val l1 = new Label
      val l2 = new Label
      mv.foreach(mv => mv.visitTryCatchBlock(l0, l1, l2, null))
      val l3 = new Label
      mv.foreach(mv => mv.visitTryCatchBlock(l2, l3, l2, null))
      codegen((synch.expr, bw))
      mv.foreach(mv => mv.visitInsn(duplicateCode(synch.expr)))
      mv.foreach(mv => mv.visitVarInsn(ASTORE, index1))
      mv.foreach(mv => mv.visitInsn(MONITORENTER))
      mv.foreach(mv => mv.visitLabel(l0))
      codegen((synch.block, bw))
      mv.foreach(mv => mv.visitVarInsn(ALOAD, index1))
      mv.foreach(mv => mv.visitInsn(MONITOREXIT))
      mv.foreach(mv => mv.visitLabel(l1))
      val l4 = new Label
      mv.foreach(mv => mv.visitJumpInsn(GOTO, l4))
      mv.foreach(mv => mv.visitLabel(l2))
      mv.foreach(mv => mv.visitVarInsn(ASTORE, index2))
      mv.foreach(mv => mv.visitVarInsn(ALOAD, index1))
      mv.foreach(mv => mv.visitInsn(MONITOREXIT))
      mv.foreach(mv => mv.visitLabel(l3))
      mv.foreach(mv => mv.visitVarInsn(ALOAD, index2))
      mv.foreach(mv => mv.visitInsn(ATHROW))
      mv.foreach(mv => mv.visitLabel(l4))
    }
  }

  protected def duplicateCode(lhs: Expr): Int = {
    CodegenUtils.duplicateCode(lhs)
  }
}


@component(tree, bw)
trait SelectCodeGenComponent extends CodeGenComponent {
  (select: SelectApi) => {
    select match {
      case Select(qual, id)     if id.name == Name("length") &&
                  qual.tpe.map(_.isInstanceOf[ArrayType]).getOrElse(false) =>
        // do array length
        codegen((qual, bw))
        bw.methodVisitor.foreach(mv => mv.visitInsn(ARRAYLENGTH))
      case _                                                               =>
        codegen((select.qual, bw))
        val shallNotPop = select.qual.symbol.map ( sym =>
          sym.isInstanceOf[ClassSymbol] ||
              sym.isInstanceOf[PackageSymbol]
        ).getOrElse(false)
        if(select.symbol.map(_.mods.isStatic).getOrElse(false) &&
              !shallNotPop) {
          bw.methodVisitor.foreach(mv => mv.visitInsn(POP))
        }
        codegen((select.tree, bw))
    }
  }
}

@component(tree, bw)
trait IdentCodeGenComponent extends CodeGenComponent {
  (id: IdentApi) => {
    val mv = bw.methodVisitor
    id.identifierIndex match {
      case Some(index)                                              =>
        if(bw.isRhs) {
          mv.foreach(mv => storeToLocalVariable(id.tpe, index,
            mv))
        } else {
          mv.foreach(mv => loadFromLocalVariable(id.tpe, index,
            mv))
        }
      case None   if id.symbol.map(_.mods.isField).getOrElse(false) =>
        if(bw.isRhs) {
          mv.foreach(mv => putField(id, mv))
        } else {
          mv.foreach(mv => getField(id, mv))
        }
      case _                                                       =>
    }
  }

  def getField(id: IdentApi, mv: MethodVisitor): Unit =
    CodegenUtils.getField(id, mv)

  def putField(id: IdentApi, mv: MethodVisitor): Unit =
    CodegenUtils.putField(id, mv)

  protected def storeToLocalVariable(tpe: Option[Type], index: Int,
        mv: MethodVisitor): Unit =
        CodegenUtils.storeToLocalVariable(tpe, index, mv, false)

  protected def loadFromLocalVariable(tpe: Option[Type], index: Int,
        mv: MethodVisitor): Unit =
        CodegenUtils.loadFromLocalVariable(tpe, index, mv, false)
}


@component(tree, bw)
trait TryCodeGenComponent extends CodeGenComponent {
  (tri: TryApi) => {
    bw.methodVisitor.foreach { mv =>
      // labels for the try-clause
      val l0 = new Label
      val l1 = new Label

      // every catch statement, has a label and a visitTryCatchBlock
      val catchLabels = tri.catches.map { ctch =>
        val start    = new Label
        val end      = new Label
        val name = ctch.eparam.tpe.map(
          toInternalTypeRepresentation(_)).getOrElse("")
        mv.visitTryCatchBlock(l0, l1, start, name)
        (start, end)
      }

      // finally (if it exists)
      val finallyLabel = tri.finallyClause.map { fin =>
        val l = new Label
        mv.visitTryCatchBlock(l0, l1, l, null)
        l
      }

      val lnext = new Label

      catchLabels.foreach { case l =>
        mv.visitTryCatchBlock(l._1, l._2, finallyLabel.getOrElse(lnext), null)
      }

      // before you start try block
      mv.visitLabel(l0)

      // body of try
      codegen((tri.tryClause, bw))
      mv.visitLabel(l1)

      // label for the end of the try-catch-finally block
      mv.visitJumpInsn(GOTO, lnext)
      // end of try


      tri.catches.zip(catchLabels).foreach { it =>
        val ctch        = it._1
        val startLabel  = it._2._1
        val endLabel    = it._2._2
        val index = ctch.eparam.variableIndex.getOrElse(0)

        // start catch body
        mv.visitLabel(startLabel)
        mv.visitVarInsn(ASTORE, index)
        //body
        codegen((ctch.catchClause, bw))
        mv.visitLabel(endLabel)
        // end catch here
        mv.visitJumpInsn(GOTO, lnext)
      }

      // finally
      for {
        fin   <- tri.finallyClause
        label <- finallyLabel
        index <- tri.finallyParamIndex
      } {
        mv.visitLabel(label)
        mv.visitVarInsn(ASTORE, index)
        mv.visitVarInsn(ALOAD, index)

        mv.visitInsn(ATHROW)
        ((fin, bw))
        // body here

        // end of try-catch
        mv.visitLabel(lnext)
      }

    }
  }

  protected def toInternalTypeRepresentation(tpe: Type): String =
    CodegenUtils.toInternalTypeRepresentation(tpe, false)
}
