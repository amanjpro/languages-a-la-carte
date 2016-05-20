package ch.usi.inf.l3.sana.guod.codegen



import ch.usi.inf.l3.sana
import sana.guod
import sana.tiny
import sana.ooj
import sana.dynj
import sana.primj
import sana.calcj



import tiny.dsl._
import tiny.core.TransformationComponent
import tiny.names.Name
import tiny.modifiers.Flags
import guod.modifiers.ModifiersUtils
import guod.ast.TreeFactories
import guod.ast.TreeExtractors._
import guod.ast._
import ooj.types.RefType
import ooj.symbols.{ClassSymbol, PackageSymbol}
import ooj.names.StdNames.CONSTRUCTOR_NAME
import primj.types.MethodType
import tiny.types.Type
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
import java.io.{FileOutputStream, BufferedOutputStream}

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

trait StackInfo {
  def resetStack(): Unit
  def incrementSP(): Unit
  def decrementSP(): Unit
  def setStack(n: Int): Unit
  def maxStack: Int
}
object StackInfo extends StackInfo {
  private[this] var stack: Int = 0

  def resetStack(): Unit = stack = 0
  def incrementSP(): Unit = stack = stack + 1
  def decrementSP(): Unit = stack = stack - 1
  def setStack(n: Int): Unit = stack = n
  def maxStack: Int = stack
}

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
    val cw   = new ClassWriter(0)
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
      val parent = ps.headOption.getOrElse(TreeFactories.mkIdent(noname))
      (parent, is)
    }
    val parentName = fullNameToInternalName(toQualifiedString(parent))
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
        s"${bw.destination}/$fullName.class"
      else s"$fullName.class"
    val byteArray = cw.toByteArray
    val bos = new BufferedOutputStream(new FileOutputStream(filename))
    Stream.continually(bos.write(byteArray))
    bos.close()
  }

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
            v.value.toDouble
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
        codegen((valdef.rhs, bw))
        mv.foreach(mv => storeToLocalVariable(valdef.tpe, index,
            StackInfo, mv))
      }
    }
  }

  protected def storeToLocalVariable(tpe: Option[Type], index: Int,
        stackInfo: StackInfo, mv: MethodVisitor): Unit =
        CodegenUtils.storeToLocalVariable(tpe, index, stackInfo, mv, false)

  protected def useTreeToInternalType(use: UseTree): String =
    CodegenUtils.useTreeToInternalType(use, true)

  protected def modifiersToBytecode(flags: Flags): Int = {
    ModifiersUtils.modifiersToBytecode(flags)
  }
}


@component(tree, bw)
trait MethodDefCodeGenComponent extends CodeGenComponent {
  (mthd: MethodDefApi) => {
    StackInfo.resetStack
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
      mv.visitMaxs(StackInfo.maxStack, maxLocals)
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
    StackInfo.incrementSP
  }
}

@component(tree, bw)
trait ThisCodeGenComponent extends CodeGenComponent {
  (ths: ThisApi)  => {
    if(!bw.isRhs) {
      bw.methodVisitor.foreach(_.visitVarInsn(ALOAD, 0))
      StackInfo.incrementSP
    }
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
      tpe1 <- cast.expr.tpe
      tpe2 <- cast.tpt.tpe
    } {
      if(tpe1 <:< LongType && tpe2 <:< IntType)
        mv.foreach(mv => mv.visitInsn(I2L))
      else if(tpe1 <:< FloatType && tpe2 <:< IntType)
        mv.foreach(mv => mv.visitInsn(I2F))
      else if(tpe1 <:< DoubleType && tpe2 <:< IntType)
        mv.foreach(mv => mv.visitInsn(I2D))
      else if(tpe1 <:< LongType && tpe2 <:< IntType)
        mv.foreach(mv => mv.visitInsn(L2I))
      else if(tpe1 <:< LongType && tpe2 <:< FloatType)
        mv.foreach(mv => mv.visitInsn(L2F))
      else if(tpe1 <:< LongType && tpe2 <:< DoubleType)
        mv.foreach(mv => mv.visitInsn(L2D))
      else if(tpe1 <:< FloatType && tpe2 <:< IntType)
        mv.foreach(mv => mv.visitInsn(F2I))
      else if(tpe1 <:< FloatType && tpe2 <:< LongType)
        mv.foreach(mv => mv.visitInsn(F2L))
      else if(tpe1 <:< FloatType && tpe2 <:< DoubleType)
        mv.foreach(mv => mv.visitInsn(F2D))
      else if(tpe1 <:< DoubleType && tpe2 <:< IntType)
        mv.foreach(mv => mv.visitInsn(D2I))
      else if(tpe1 <:< DoubleType && tpe2 <:< LongType)
        mv.foreach(mv => mv.visitInsn(D2L))
      else if(tpe1 <:< DoubleType && tpe2 <:< FloatType)
        mv.foreach(mv => mv.visitInsn(D2F))
      else if(tpe1.isInstanceOf[RefType] && tpe2.isInstanceOf[RefType]) {
        val name = useTreeToInternalType(cast.tpt)
        mv.foreach(mv => mv.visitTypeInsn(CHECKCAST, name))
      }
    }
  }

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
        codegen((bin.rhs, bw))
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
    StackInfo.decrementSP
  }


  protected def useTreeToInternalType(use: UseTree): String =
    CodegenUtils.useTreeToInternalType(use, false)

  def codegenArith(bin: BinaryApi, bw: ByteCodeWriter): Unit = {
    codegen((bin.lhs, bw))
    codegen((bin.rhs, bw))
    val mv = bw.methodVisitor
    (bin.tpe, bin.op) match {
      case (Some(tpe), Add) if tpe <:< IntType           =>
        mv.foreach(mv => mv.visitInsn(IADD))
      case (Some(tpe), Add) if tpe <:< LongType          =>
        mv.foreach(mv => mv.visitInsn(LADD))
      case (Some(tpe), Add) if tpe <:< FloatType         =>
        mv.foreach(mv => mv.visitInsn(FADD))
      case (Some(tpe), Add) if tpe <:< DoubleType        =>
        mv.foreach(mv => mv.visitInsn(DADD))
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
          StackInfo.incrementSP
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
          StackInfo.incrementSP
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
          StackInfo.incrementSP
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
          StackInfo.incrementSP
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
          StackInfo.incrementSP
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
          StackInfo.incrementSP
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
      case (Some(tpe), Not)                              =>
        codegen((unary.expr, bw))
        val l0 = new Label
        mv.foreach(mv => mv.visitJumpInsn(IFNE, l0))
        mv.foreach(mv => mv.visitInsn(ICONST_1))
        val l1 = new Label
        mv.foreach(mv => mv.visitJumpInsn(GOTO, l1))
        mv.foreach(mv => mv.visitLabel(l0))
        mv.foreach(mv => mv.visitInsn(ICONST_0))
        mv.foreach(mv => mv.visitLabel(l1))
        StackInfo.incrementSP
      case (Some(tpe), BCompl)                           =>
        codegen((unary.expr, bw))
        mv.foreach(mv => mv.visitInsn(ICONST_M1))
        mv.foreach(mv =>  mv.visitInsn(IXOR))
        StackInfo.incrementSP
      case (Some(tpe), Inc)     if unary.isPostfix       =>
        val (one, op) = oneAndOp(unary.tpe, true)
        codegen((unary.expr, bw))
        mv.foreach(mv => mv.visitInsn(one))
        mv.foreach(mv => mv.visitInsn(op))
        codegen((unary.expr, bw.copy(isRhs = true)))
      case (Some(tpe), Inc)                              =>
        val (one, op) = oneAndOp(unary.tpe, true)
        codegen((unary.expr, bw))
        mv.foreach(mv => mv.visitInsn(one))
        mv.foreach(mv => mv.visitInsn(op))
        codegen((unary.expr, bw.copy(isRhs = true)))
      case (Some(tpe), Dec)     if unary.isPostfix       =>
        val (one, op) = oneAndOp(unary.tpe, false)
        codegen((unary.expr, bw))
        mv.foreach(mv => mv.visitInsn(one))
        mv.foreach(mv => mv.visitInsn(op))
        codegen((unary.expr, bw.copy(isRhs = true)))
      case (Some(tpe), Dec)                              =>
        val (one, op) = oneAndOp(unary.tpe, false)
        codegen((unary.expr, bw))
        mv.foreach(mv => mv.visitInsn(one))
        mv.foreach(mv => mv.visitInsn(op))
        codegen((unary.expr, bw.copy(isRhs = true)))
      case (Some(tpe), Neg) if tpe <:< IntType           =>
        mv.foreach(mv => mv.visitInsn(INEG))
      case (Some(tpe), Neg) if tpe <:< LongType          =>
        mv.foreach(mv => mv.visitInsn(LNEG))
      case (Some(tpe), Neg) if tpe <:< FloatType         =>
        mv.foreach(mv => mv.visitInsn(FNEG))
      case (Some(tpe), Neg) if tpe <:< DoubleType        =>
        mv.foreach(mv => mv.visitInsn(DNEG))
      case (Some(tpe), Pos)                              =>
        ()
      case _                                             =>
        ()
    }
  }



  protected def oneAndOp(tpe: Option[Type],
          isAdd: Boolean): (Int, Int) = tpe match {
    case Some(tpe)    if tpe <:< IntType && isAdd      => (ICONST_1, IADD)
    case Some(tpe)    if tpe <:< IntType               => (ICONST_1, ISUB)
    case Some(tpe)    if tpe <:< LongType && isAdd     => (LCONST_1, LADD)
    case Some(tpe)    if tpe <:< LongType              => (LCONST_1, LSUB)
    case Some(tpe)    if tpe <:< FloatType && isAdd    => (FCONST_1, FADD)
    case Some(tpe)    if tpe <:< FloatType             => (FCONST_1, FSUB)
    case Some(tpe)    if tpe <:< DoubleType && isAdd   => (DCONST_1, DADD)
    case Some(tpe)    if tpe <:< DoubleType            => (DCONST_1, DADD)

  }
}


@component(tree, bw)
trait IfCodeGenComponent extends CodeGenComponent {
  (ifelse: IfApi) => {
    codegen((ifelse.cond, bw))
    val mv = bw.methodVisitor
    val l0 = new Label
    mv.foreach(mv => mv.visitJumpInsn(IFNE, l0))
    val condStack = StackInfo.maxStack
    codegen((ifelse.thenp, bw))
    val thenStack = StackInfo.maxStack
    StackInfo.setStack(condStack)
    val l1 = new Label
    mv.foreach(mv => mv.visitJumpInsn(GOTO, l1))
    mv.foreach(mv => mv.visitLabel(l0))
    codegen((ifelse.elsep, bw))
    val elseStack = StackInfo.maxStack
    StackInfo.setStack(thenStack.max(elseStack))
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
    val condStack = StackInfo.maxStack
    codegen((tern.thenp, bw))
    val thenStack = StackInfo.maxStack
    val l1 = new Label
    mv.foreach(mv => mv.visitJumpInsn(GOTO, l1))
    mv.foreach(mv => mv.visitLabel(l0))
    StackInfo.setStack(condStack)
    codegen((tern.elsep, bw))
    val elseStack = StackInfo.maxStack
    StackInfo.setStack(thenStack.max(elseStack))
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
    }

    mv.foreach(mv => mv.visitLabel(cond))
    codegen((forloop.cond, bw))
    mv.foreach(mv => mv.visitJumpInsn(IFEQ, body))

    mv.foreach(mv => mv.visitLabel(next))
  }
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
    val defaultCase  = switch.cases.filter(_ == Nil)
    val defaultLabel = defaultCase match {
      case Nil                  => next
      case _                    => new Label
    }
    val (guards, allLabels) = {
      val res = nonDefaultCases.zip(labels).flatMap { it =>
        val cse = it._1
        val lbl = it._2
        cse.guards.map { guard =>
          (guard.asInstanceOf[LiteralApi].constant.value.asInstanceOf[Int],
            lbl)
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
        codegen((cse, bw.copy(breakTarget = Some(next))))
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
        mv.foreach(mv => mv.visitLdcInsn(v))
      case Literal(NullConstant)                  =>
        mv.foreach(mv => mv.visitInsn(ACONST_NULL))
    }
    StackInfo.incrementSP
  }
}



@component(tree, bw)
trait BlockCodeGenComponent extends CodeGenComponent {
  (block: BlockApi) => {
    block.stmts.foreach(stmt => codegen((stmt, bw)))
  }
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
    elementTypeToOpcode(init.tpe) match {
      case Right(opcode)           =>
        mv.foreach(mv => mv.visitIntInsn(NEWARRAY, opcode))
      case Left(tpe)               =>
        mv.foreach(mv => mv.visitTypeInsn(NEWARRAY, tpe))
    }
    val elements = init.elements.zipWithIndex
    elements.foreach { e =>
      val element = e._1
      val index   = e._2
      mv.foreach(mv => mv.visitLdcInsn(index))
      mv.foreach(mv => mv.visitLdcInsn(element))
      mv.foreach(mv => storeToLocalVariable(element.tpe,
            StackInfo, mv))
      StackInfo.incrementSP
    }
  }

  protected def storeToLocalVariable(tpe: Option[Type],
        stackInfo: StackInfo, mv: MethodVisitor): Unit =
        CodegenUtils.storeToLocalVariable(tpe, -1, stackInfo, mv, true)

  protected def elementTypeToOpcode(etpe: Option[Type]): Either[String, Int] =
    CodegenUtils.elementTypeToOpcode(etpe)
}


@component(tree, bw)
trait ArrayAccessCodeGenComponent extends CodeGenComponent {
  (access: ArrayAccessApi)               => {
    val mv = bw.methodVisitor
    StackInfo.incrementSP
    codegen((access.array, bw.copy(isRhs = false)))
    codegen((access.index, bw.copy(isRhs = false)))
    if(bw.isRhs) {
      mv.foreach(mv => storeToLocalVariable(access.tpe, StackInfo, mv))
      StackInfo.decrementSP
      StackInfo.decrementSP
    } else {
      mv.foreach(mv => loadFromLocalVariable(access.tpe, StackInfo, mv))
      StackInfo.decrementSP
    }
  }

  protected def storeToLocalVariable(tpe: Option[Type],
        stackInfo: StackInfo, mv: MethodVisitor): Unit =
        CodegenUtils.storeToLocalVariable(tpe, -1, stackInfo, mv, true)

  protected def loadFromLocalVariable(tpe: Option[Type],
        stackInfo: StackInfo, mv: MethodVisitor): Unit =
        CodegenUtils.loadFromLocalVariable(tpe, -1, stackInfo, mv, true)
}

@component(tree, bw)
trait AssignCodeGenComponent extends CodeGenComponent {
  (assign: AssignApi) => {
    assign.lhs match {
      // case ArrayAccess(Select(q: ThisApi, t), i)                  =>
      //   codegen((q, bw))
      //   codegen((i, bw))
      //   codegen((assign.rhs, bw))
      //   codegen((t, bw.copy(isRhs = true)))
      case ArrayAccess(a, i)                                      =>
        codegen((a, bw))
        codegen((i, bw))
        codegen((assign.rhs, bw))
        val mv = bw.methodVisitor
        mv.foreach(mv => storeToLocalVariable(assign.lhs.tpe, StackInfo, mv))
        StackInfo.decrementSP
        StackInfo.decrementSP
      case Select(q: ThisApi, t)                                  =>
        codegen((q, bw))
        codegen((assign.rhs, bw))
        codegen((t, bw.copy(isRhs = true)))
      case lhs                                                    =>
        codegen((assign.rhs, bw))
        codegen((lhs, bw.copy(isRhs = true)))
    }
  }

  protected def storeToLocalVariable(tpe: Option[Type],
        stackInfo: StackInfo, mv: MethodVisitor): Unit =
        CodegenUtils.storeToLocalVariable(tpe, -1, stackInfo, mv, true)

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
    StackInfo.incrementSP
    if(app.fun.symbol.map(_.mods.isStatic).getOrElse(false)) {
      app.args.foreach(arg => codegen((arg, bw)))
      mv.foreach( mv =>
          mv.visitMethodInsn(INVOKESTATIC, className, funName, funSig, false))
      StackInfo.incrementSP
    } else {
      app.args.foreach { arg =>
        codegen((arg, bw))
        StackInfo.decrementSP
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
    StackInfo.incrementSP
    mv.foreach(mv => mv.visitInsn(DUP))
    StackInfo.incrementSP
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
      mv.foreach(mv => mv.visitInsn(DUP))
      StackInfo.incrementSP
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
      StackInfo.incrementSP
    }
  }
}


@component(tree, bw)
trait SelectCodeGenComponent extends CodeGenComponent {
  (select: SelectApi) => {
    codegen((select.qual, bw))
    val shallNotPop = select.qual.symbol.map ( sym =>
      sym.isInstanceOf[ClassSymbol] ||
          sym.isInstanceOf[PackageSymbol]
    ).getOrElse(false)
    if(select.symbol.map(_.mods.isStatic).getOrElse(false) &&
          !shallNotPop) {
      bw.methodVisitor.foreach(mv => mv.visitInsn(POP))
      StackInfo.decrementSP
    }
    codegen((select.tree, bw))
  }


  def isInConstructor(owner: Option[Symbol]): Boolean =
    SymbolUtils.enclosingMethod(owner).map(
      _.mods.isConstructor).getOrElse(false)
}

@component(tree, bw)
trait IdentCodeGenComponent extends CodeGenComponent {
  (id: IdentApi) => {
    val mv = bw.methodVisitor
    id.identifierIndex match {
      case Some(index)                                              =>
        if(bw.isRhs) {
          mv.foreach(mv => storeToLocalVariable(id.tpe, index,
            StackInfo, mv))
          StackInfo.decrementSP
        } else {
          mv.foreach(mv => loadFromLocalVariable(id.tpe, index,
            StackInfo, mv))
          StackInfo.incrementSP
        }
      case None   if id.symbol.map(_.mods.isField).getOrElse(false) =>
        val isStatic  = id.symbol.map(_.mods.isStatic).getOrElse(false)
        val className = toFullyQualifiedTypeName(
          enclosingClass(id.owner)).replaceAll("[.]", "/")
        val tpe       = id.tpe.map(tpe =>
            toInternalTypeRepresentation(tpe)).getOrElse("")
        if(bw.isRhs) {
          if(isStatic)
            mv.foreach(mv => mv.visitFieldInsn(PUTSTATIC,
              className, id.name.asString, tpe))
          else
            mv.foreach(mv => mv.visitFieldInsn(PUTFIELD, className,
              id.name.asString, tpe))
          StackInfo.decrementSP
        } else {
          if(isStatic)
            mv.foreach(mv => mv.visitFieldInsn(GETSTATIC,
              className, id.name.asString, tpe))
          else
            mv.foreach(mv => mv.visitFieldInsn(GETFIELD, className,
              id.name.asString, tpe))
          StackInfo.incrementSP
        }
      case _                                                       =>
    }
  }

  protected def enclosingClass(owner: Option[Symbol]): Option[Symbol] =
    SymbolUtils.enclosingClass(owner)

  protected def toFullyQualifiedTypeName(symbol: Option[Symbol]): String =
    SymbolUtils.toFullyQualifiedTypeName(symbol)

  protected def toInternalTypeRepresentation(tpe: Type): String =
    CodegenUtils.toInternalTypeRepresentation(tpe, true)

  protected def storeToLocalVariable(tpe: Option[Type], index: Int,
        stackInfo: StackInfo, mv: MethodVisitor): Unit =
        CodegenUtils.storeToLocalVariable(tpe, index, stackInfo, mv, false)

  protected def loadFromLocalVariable(tpe: Option[Type], index: Int,
        stackInfo: StackInfo, mv: MethodVisitor): Unit =
        CodegenUtils.loadFromLocalVariable(tpe, index, stackInfo, mv, false)
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
        StackInfo.decrementSP
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
