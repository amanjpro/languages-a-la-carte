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

package ch.usi.inf.l3.sana.modulej.parser

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.brokenj
import sana.ooj
import sana.arrayj
import sana.arrooj
import sana.dynj
import sana.robustj
import sana.ppj
import sana.modulej


import tiny.source.SourceFile
import tiny.source.Position
import tiny.symbols._
import tiny.parsers
import tiny.ast.{TreeFactories => _, TreeCopiers => _, _}
import tiny.modifiers.Flags
import tiny.names.Name


import calcj.ast.{TreeFactories => _, TreeCopiers => _, _}
import primj.modifiers._
import primj.ast.{MethodDefApi => _, TreeFactories => _,
  TreeCopiers =>_,  _}
import brokenj.ast.{TreeFactories => _, TreeCopiers => _, _}
import ooj.modifiers._
import modulej.modifiers.Ops._

import ooj.ast.Implicits._
import calcj.ast.operators._
import ooj.types._
import ooj.names.StdNames._
import ooj.ast.{TreeFactories => _, MethodDefApi => _,
                CompilationUnitApi => _, TreeCopiers => _, _}
import ooj.antlr._
import ooj.ast.TreeExtractors._

import modulej.ast._
import robustj.ast.{TreeFactories => _, TreeCopiers => _, _}

import modulej.modifiers._
import ppj.modifiers._
import robustj.modifiers._
import dynj.ast.operators._

import arrayj.ast.{TreeFactories => _, TreeCopiers => _, _}

import org.antlr.v4.runtime.misc.NotNull
import org.antlr.v4.runtime.{ParserRuleContext, Token}
import org.antlr.v4.runtime.Token
import org.antlr.v4.runtime.tree.{AbstractParseTreeVisitor, TerminalNode}

import org.apache.commons.lang3.StringEscapeUtils

import scala.collection.JavaConverters._

class Parser extends parsers.Parser {


  def parse(source: SourceFile): Tree = {
    val unit = new ModulejVisitor(source.fileName, source.name,
      source.lines).visit(source.content).asInstanceOf[CompilationUnitApi]
    TreeCopiers.copyCompilationUnit(unit)(sourcePath = source.filePath)
  }

  class ModulejVisitor(val fileName: String, val fullName: String,
        lines: Array[String]) extends Java1BaseVisitor[Tree] {

    def pos(token: Token): Option[Position] = {
      Some(Position(fullName,
        lines, token.getLine, token.getCharPositionInLine + 1))
    }

    def pos(ctx: ParserRuleContext): Option[Position] = {
      val token = ctx.getStart
      Some(Position(fullName, lines,
        token.getLine, token.getCharPositionInLine + 1))
    }

    def localVariableDeclaration(
      ctx: Java1Parser.LocalVariableDeclarationContext): List[ValDefApi] = {
      // Java1 does not allow modifiers on local variables,
      // Because there were no inner class back then
      val mods       = noflags | LOCAL_VARIABLE
      val tpt        = visit(ctx.`type`()).asInstanceOf[UseTree]
      ctx.variableDeclarators.variableDeclarator.asScala.toList.map {
        case ctx =>
          val tpt2   =
            dimsToArrayType(tpt, ctx.variableDeclaratorId.dims)
          val name   = Name(ctx.variableDeclaratorId.Identifier.getText)
          val rhs    = ctx.variableInitializer match {
            case null          => NoTree
            case child         => visit(child).asInstanceOf[Expr]
          }
          TreeFactories.mkValDef(mods, tpt2, name, rhs, pos(ctx))
      }
    }


    def modifierToFlag(ctx: Java1Parser.ModifierContext): Flags =
      ctx.getText match {
        case "public"                     => Flags(PUBLIC_ACC)
        case "protected"                  => Flags(PROTECTED_ACC)
        case "private"                    => Flags(PRIVATE_ACC)
        case "static"                     => Flags(STATIC)
        case "abstract"                   => Flags(ABSTRACT)
        case "final"                      => Flags(FINAL)
        case "native"                     => Flags(NATIVE)
        case "synchronized"               => Flags(SYNCHRONIZED)
        case "transient"                  => Flags(TRANSIENT)
        case "volatile"                   => Flags(VOLATILE)
      }


    def interfacesContextToTypeUses(interfaceContext:
        Java1Parser.ExtendsInterfacesContext): List[UseTree] =
      interfaceContext match {
        case null                       => Nil
        case ctx                        =>
          interfacesContextToTypeUses(ctx.classOrInterfaceTypeList)
    }


    def interfacesContextToTypeUses(interfaceContext:
      Java1Parser.InterfacesContext): List[UseTree] =
      interfaceContext match {
        case null                     => Nil
        case ctx                      =>
          interfacesContextToTypeUses(ctx.classOrInterfaceTypeList)
      }

    def interfacesContextToTypeUses(interfaces:
      Java1Parser.ClassOrInterfaceTypeListContext):
        List[UseTree] = interfaces
                        .classOrInterfaceType
                        .asScala
                        .toList
                        .map {
                          case ctx =>
                            visit(ctx).asInstanceOf[UseTree]
                        }

    def dimsToArrayType(use: UseTree,
        dims: Java1Parser.DimsContext): UseTree = dims match {
        case null                           => use
        case n                              =>
          dimsToArrayType(use, n.dim.size)
      }

    def dimsToArrayType(use: UseTree, n: Int): UseTree =
      (1 to n).foldLeft(use)((z, y) => {
        arrayj.ast.TreeFactories.mkArrayTypeUse(z)
      })

    def modifiersTo(modifiers:
      java.util.List[Java1Parser.ModifierContext],
      packageIsDefault: Boolean = false):  Flags =
        modifiers match {
          case null if packageIsDefault    => Flags(PACKAGE_ACC)
          case null                        => noflags
          case mods                        =>
            mods.asScala.toList match {
              case Nil  if packageIsDefault => Flags(PACKAGE_ACC)
              case mods                     =>
                mods.foldLeft(noflags)((z, y) =>
                    z | modifierToFlag(y))
            }
        }


    def namesToTree(names: List[TerminalNode]): Tree =
      names match {
      case Nil                                   => ErrorTree
      case List(n)                               =>
        TreeFactories.mkIdent(Name(n.getText), pos(n.getSymbol))
      case (x::xs)                               =>
        val qual = namesToTree(xs)
        val sym  = x.getSymbol
        val nme  = x.getText
        val p    = pos(sym)
        TreeFactories.mkSelect(qual,
          TreeFactories.mkIdent(Name(nme), p), p)
    }

    def createUnaryOrPostfix[T <: ParserRuleContext](isPostfix: Boolean,
      exp: T, trm: String, ctx: ParserRuleContext): Expr = {

        val e1 = visitChildren(exp).asInstanceOf[Expr]
        val op = trm match {
          case "-"     => Neg
          case "+"     => Pos
          case "++"    => Inc
          case "--"    => Dec
          case "~"     => BCompl
          case "!"     => Not
        }
        if(isPostfix)
          TreeFactories.mkUnary(true, op, e1, pos(ctx))
        else
          TreeFactories.mkUnary(false, op, e1, pos(ctx))
    }

    def createBinary[T <: ParserRuleContext](es: java.util.List[T],
      trm: String, ctx: ParserRuleContext): BinaryApi = {
      val e1 = visit(es.get(0)).asInstanceOf[Expr]
      val e2 = visit(es.get(1)).asInstanceOf[Expr]
      createBinary(e1, e2, trm, ctx)

    }

    def createBinary(e1: Expr, e2: Expr,
      trm: String, ctx: ParserRuleContext): BinaryApi = {
        val op = trm match {
          case "*"             => Mul
          case "/"             => Div
          case "%"             => Mod
          case "+"             => Add
          case "-"             => Sub
          case "<<"            => SHL
          case ">>"            => SHR
          case ">>>"           => USHR
          case "<"             => Lt
          case ">"             => Gt
          case "<="            => Le
          case ">="            => Ge
          case "=="            => Eq
          case "!="            => Neq
          case "&"             => BAnd
          case "^"             => BXor
          case "|"             => BOr
          case "&&"            => And
          case "||"            => Or
          case "instanceof"    => InstanceOf
        }
        TreeFactories.mkBinary(e1, op, e2, pos(ctx))
    }


    override def visitCompilationUnit(ctx:
        Java1Parser.CompilationUnitContext): Tree = {
      val (containingPackages, pkgName) = {
        if(ctx.packageDeclaration == null) (Nil, DEFAULT_PACKAGE_NAME)
        else {
          val raw = ctx.packageDeclaration.Identifier
          raw.asScala.toList.reverse match {
            case x::Nil          =>
              (Nil, Name(x.getText))
            case (x::xs)         =>
              (xs.map(x => Name(x.getText)).reverse, Name(x.getText))
            case _               =>
              // should never happen
              (Nil, DEFAULT_PACKAGE_NAME)
          }
        }
      }
      val imports = ctx.importDeclaration match {
        case null                           => Nil
        case imports                        =>
          imports.asScala.toList.map(visit(_).asInstanceOf[ImportApi])
      }
      val members = ctx.typeDeclaration match {
        case null                           => Nil
        case types                          =>
          types.asScala.toList.map((x) => visit(x))
      }
      val pkg = TreeFactories.mkPackageDef(containingPackages,
        pkgName, members, pos(ctx))

      TreeFactories.mkCompilationUnit(imports,
        pkg, fileName, Nil)
    }

    protected def toInt(str: String): Int = try {
      Integer.decode(str).intValue
    } catch {
      case ex: Exception          =>
        if(isBinary(str)) {
          Integer.parseInt(str.substring(2), 2)
        } else {
          // INFO: This branch is taken from OpenJDK's Java's compiler:
          // javac/util/Convert.java
          // url: http://hg.openjdk.java.net/jdk6/jdk6/langtools/file/2a66a69ffe48/src/share/classes/com/sun/tools/javac/util/Convert.java
          val chars = str.toList.tail.tail
          val radix = 16
          val limit = Integer.MAX_VALUE / (radix/2);
          var n = 0;
          chars.foldLeft(0)((z, y) => {
            val d = java.lang.Character.digit(y, 16)
            if(z <0 ||
                z > limit ||
                n * radix > Integer.MAX_VALUE - d)
                  throw new NumberFormatException();
            n * radix + d;
          })
        }
    }

    def toLong(str: String): Long = {
      try {
        java.lang.Long.decode(str).longValue
      } catch {
        case ex: Exception          =>
          // INFO: This branch is taken from OpenJDK's Java's compiler:
          // javac/util/Convert.java
          // url: http://hg.openjdk.java.net/jdk6/jdk6/langtools/file/2a66a69ffe48/src/share/classes/com/sun/tools/javac/util/Convert.java
          val chars = str.toList.tail.tail
          val radix = 16
          val limit: Long = java.lang.Long.MAX_VALUE / (radix/2);
          chars.foldLeft(0L)((z, y) => {
              val d: Long = java.lang.Character.digit(y, radix);
              if (z < 0L ||
                  z > limit ||
                  z * radix > java.lang.Long.MAX_VALUE - d) {
                  throw new NumberFormatException();
                }
              z * radix + d;
          })
      }
    }

    protected def isBinary(str: String): Boolean =
      str.startsWith("0b") && str.substring(2).exists { ch =>
        !(ch == '1' || ch == '0')
      }

    override def visitIntLit(ctx: Java1Parser.IntLitContext): Tree = {
      val txt = ctx.getText
      (txt.endsWith("l") || txt.endsWith("L")) match {
        case true  =>
          TreeFactories.mkLiteral(
            LongConstant(toLong(txt.dropRight(1))), pos(ctx))
        case false =>
          val v = toInt(txt)
          TreeFactories.mkLiteral(IntConstant(v), pos(ctx))
      }
    }


    override def visitCharLit(ctx: Java1Parser.CharLitContext): Tree = {
      val str  = ctx.getText
      val size = str.length
      val char = StringEscapeUtils.unescapeJava(str.substring(1, size -1))
      val ch = char.toCharArray.apply(0)
      TreeFactories.mkLiteral(CharConstant(ch), pos(ctx))
    }

    override def visitDoubleLit(ctx: Java1Parser.DoubleLitContext): Tree = {
      val txt = ctx.getText
      (txt.endsWith("f") || txt.endsWith("F")) match {
        case true  =>
          val v = txt.dropRight(1).toFloat
          TreeFactories.mkLiteral(FloatConstant(v), pos(ctx))
        case false =>
          val v = txt.toDouble
          TreeFactories.mkLiteral(DoubleConstant(v), pos(ctx))
      }
    }

    override def visitBoolLit(ctx: Java1Parser.BoolLitContext): Tree = {
      val b = ctx.getText match {
        case "true"           => true
        case _                => false
      }
      TreeFactories.mkLiteral(BooleanConstant(b), pos(ctx))
    }


    override def visitStringLit(ctx: Java1Parser.StringLitContext): Tree = {
      TreeFactories.mkLiteral(StringConstant(ctx.getText), pos(ctx))
    }

    override def visitNullLit(ctx: Java1Parser.NullLitContext): Tree = {
      TreeFactories.mkLiteral(NullConstant, pos(ctx))
    }


    override def visitType(ctx: Java1Parser.TypeContext): Tree = {
      if(ctx.referenceType == null)
        visit(ctx.primitiveType)
      else visit(ctx.referenceType)
    }

    override def visitPrimitiveType(ctx:
      Java1Parser.PrimitiveTypeContext): Tree = ctx.getText match {
      case "byte"                =>
        TreeFactories.mkTypeUse(BYTE_TYPE_NAME, pos(ctx))
      case "short"               =>
        TreeFactories.mkTypeUse(SHORT_TYPE_NAME, pos(ctx))
      case "char"                =>
        TreeFactories.mkTypeUse(CHAR_TYPE_NAME, pos(ctx))
      case "int"                 =>
        TreeFactories.mkTypeUse(INT_TYPE_NAME, pos(ctx))
      case "long"                =>
        TreeFactories.mkTypeUse(LONG_TYPE_NAME, pos(ctx))
      case "float"               =>
        TreeFactories.mkTypeUse(FLOAT_TYPE_NAME, pos(ctx))
      case "double"              =>
        TreeFactories.mkTypeUse(DOUBLE_TYPE_NAME, pos(ctx))
      case "boolean"             =>
        TreeFactories.mkTypeUse(BOOLEAN_TYPE_NAME, pos(ctx))
    }

    override def visitReferenceType(ctx:
      Java1Parser.ReferenceTypeContext): Tree = {
      if(ctx.arrayType == null)
        visit(ctx.classOrInterfaceType)
      else visit(ctx.arrayType)
    }



    override def visitClassOrInterfaceType(ctx:
      Java1Parser.ClassOrInterfaceTypeContext): Tree = {
      visitChildren(ctx) match {
        case id: IdentApi                                  =>
          TreeFactories.mkTypeUse(id.name, id.pos)
        case s: SelectApi                                  =>
          s.tree match {
            case id: IdentApi   =>
              val tuse =
                TreeFactories.mkTypeUse(id.name, id.pos)
              TreeFactories.mkSelect(s.qual, tuse, s.pos)
            case _              =>
              s
          }
        case t                                             =>
          t
      }
    }


    override def visitArrayArrayType(ctx: Java1Parser.ArrayArrayTypeContext): Tree = {
      val use = visit(ctx.arrayType).asInstanceOf[UseTree]
      dimsToArrayType(use, 1)
    }

    override def visitPrimitiveArrayType(ctx: Java1Parser.PrimitiveArrayTypeContext): Tree = {
      val use = visit(ctx.primitiveType).asInstanceOf[UseTree]
      dimsToArrayType(use, 1)
    }

    override def visitNameArrayType(ctx: Java1Parser.NameArrayTypeContext): Tree = {
      val use = visit(ctx.name).asInstanceOf[UseTree]
      dimsToArrayType(use, 1)
    }


    override def visitName(ctx: Java1Parser.NameContext): Tree = {
      namesToTree(ctx.Identifier.asScala.toList.reverse)
    }

    override def visitPackageDeclaration(ctx:
      Java1Parser.PackageDeclarationContext): Tree = {
      // INFO: Do not use this method
      visitChildren(ctx)
    }

    override def visitImportDeclaration(ctx:
      Java1Parser.ImportDeclarationContext): Tree = {
      // INFO: Do not use this method
      visitChildren(ctx)
    }

    override def visitSingleTypeImportDeclaration(ctx:
      Java1Parser.SingleTypeImportDeclarationContext): Tree = {
      val name = visit(ctx.name) match {
        case s@Select(q, id: IdentApi) =>
          val tuse = TreeFactories.mkTypeUse(id.name, id.pos)
          TreeCopiers.copySelect(s)(tree = tuse)
        case t                         =>
          t.asInstanceOf[UseTree]
      }
      TreeFactories.mkImport(name, false, pos(ctx))
    }


    override def visitTypeImportOnDemandDeclaration(ctx:
      Java1Parser.TypeImportOnDemandDeclarationContext): Tree = {
      val name = visit(ctx.name).asInstanceOf[UseTree]
      TreeFactories.mkImport(name, true, pos(ctx))
    }

    override def visitTypeDeclaration(ctx:
      Java1Parser.TypeDeclarationContext): Tree = {
      if(ctx.classDeclaration == null)
        visit(ctx.interfaceDeclaration)
      else visit(ctx.classDeclaration)
    }

    override def visitClassDeclaration(ctx:
      Java1Parser.ClassDeclarationContext): Tree = {
      val mods       = modifiersTo(ctx.modifier, true) | CLASS
      val name       = Name(ctx.Identifier.getText)
      val parent     = ctx.parent() match {
        case null                  =>
          None
        case _                     =>
          val res =
            visit(ctx.parent().classOrInterfaceType()).asInstanceOf[UseTree]
          res match {
            case tuse: TypeUseApi                =>
              tuse.isInExtendsClause = true
            case Select(_, tuse: TypeUseApi)     =>
              tuse.isInExtendsClause = true
            case _                               =>
              ()
          }
          Some(res)
      }
      val body       = visit(ctx.classBody()).asInstanceOf[TemplateApi]
      val interfaces = interfacesContextToTypeUses(ctx.interfaces())
      interfaces.foreach { t => t match {
          case tuse: TypeUseApi                =>
            tuse.isInImplementsClause = true
          case Select(_, tuse: TypeUseApi)     =>
            tuse.isInImplementsClause = true
          case _                               =>
            ()
        }
      }
      val res = TreeFactories.mkClassDef(mods, name,
        parent.toList ++ interfaces, body, pos(ctx))
      if(mods.isPublicAcc)
        res.sourceName = fileName
      res
    }


    override def visitClassBody(ctx: Java1Parser.ClassBodyContext): Tree = {
      ctx.classBodyDeclaration() match {
        case null                     =>
          TreeFactories.mkTemplate(Nil, pos(ctx))
        case body                     =>
          val members = body.asScala.toList.flatMap { (x) =>
            if(x.classMemberDeclaration != null &&
                x.classMemberDeclaration.fieldDeclaration != null) {
              val ctx = x.classMemberDeclaration.fieldDeclaration
              val mods       = modifiersTo(ctx.modifier, true) | FIELD
              val tpt        = visit(ctx.`type`()).asInstanceOf[UseTree]
              ctx.variableDeclarators.variableDeclarator.asScala.toList.map {
                case ctx =>
                  val tpt2   =
                    dimsToArrayType(tpt, ctx.variableDeclaratorId.dims)
                  val name   = Name(ctx.variableDeclaratorId.Identifier.getText)
                  val rhs    = ctx.variableInitializer match {
                    case null          => NoTree
                    case child         => visit(child).asInstanceOf[Expr]
                  }
                  TreeFactories.mkValDef(mods, tpt2, name, rhs, pos(ctx))
              }
            } else {
              List(visit(x))
            }
          }
          TreeFactories.mkTemplate(members, pos(ctx))
      }
    }

    override def visitClassBodyDeclaration(ctx:
      Java1Parser.ClassBodyDeclarationContext): Tree = {
      if(ctx.classMemberDeclaration != null)
        visit(ctx.classMemberDeclaration)
      else if(ctx.staticInitializer != null)
        visit(ctx.staticInitializer)
      else
        visit(ctx.constructorDeclaration)
    }


    override def visitClassMemberDeclaration(ctx:
      Java1Parser.ClassMemberDeclarationContext): Tree = {
      if(ctx.fieldDeclaration == null)
        visit(ctx.methodDeclaration)
      else visit(ctx.fieldDeclaration)
    }

    override def visitFieldDeclaration(ctx:
      Java1Parser.FieldDeclarationContext): Tree = {
      // INFO: Don't implement this
      visitChildren(ctx)
    }

    override def visitVariableDeclarators(ctx:
      Java1Parser.VariableDeclaratorsContext): Tree = {
      // INFO: Don't implement this
      visitChildren(ctx)
    }

    override def visitVariableDeclarator(ctx:
      Java1Parser.VariableDeclaratorContext): Tree = {
      // INFO: Don't implement this
      visitChildren(ctx)
    }

    override def visitVariableDeclaratorId(ctx:
      Java1Parser.VariableDeclaratorIdContext): Tree = {
      // INFO: Don't implement this
      visitChildren(ctx)
    }

    override def visitVariableInitializer(ctx:
      Java1Parser.VariableInitializerContext): Tree = {
      // INFO: Don't implement this
      visitChildren(ctx)
    }

    def parseThrowsClause(ctx: Java1Parser.TypeMethodHeaderContext):
      List[UseTree] = ctx.throwsClause match {
      case null                           => Nil
      case tc                             =>
        tc.classOrInterfaceTypeList
          .classOrInterfaceType.asScala.
          toList.map(visit(_).asInstanceOf[UseTree])
    }

    def parseThrowsClause(ctx: Java1Parser.VoidMethodHeaderContext):
      List[UseTree] = ctx.throwsClause match {
      case null                           => Nil
      case tc                             =>
        tc.classOrInterfaceTypeList
          .classOrInterfaceType.asScala.
          toList.map(visit(_).asInstanceOf[UseTree])
    }

    def parseThrowsClause(ctx: Java1Parser.MethodDeclarationContext):
      List[UseTree] = if(ctx.typeMethodHeader == null) {
          parseThrowsClause(ctx.voidMethodHeader)
        } else {
          parseThrowsClause(ctx.typeMethodHeader)
        }

    override def visitMethodDeclaration(ctx:
      Java1Parser.MethodDeclarationContext): Tree = {
      val mthd = if(ctx.typeMethodHeader == null)
        visit(ctx.voidMethodHeader)
      else visit(ctx.typeMethodHeader)
      mthd match {
        case md: MethodDefApi                     =>
          val throwsClause = parseThrowsClause(ctx)
          val body = visit(ctx.methodBody).asInstanceOf[Expr]
          TreeFactories.mkMethodDef(md.mods, md.ret, md.name,
            md.params, throwsClause, body, md.pos)
        case t                                    =>
          t
      }
    }

    def formalParameterListToValDefs(ctx:
      Java1Parser.FormalParameterListContext): List[ValDefApi] = {
      ctx match {
        case null                     => Nil
        case ctx                      =>
          ctx.formalParameter().asScala.toList.map ((ctx) => {
            val tpt  = {
              val tpt = visit(ctx.`type`()).asInstanceOf[UseTree]
              val dims = ctx.variableDeclaratorId.dims
              dimsToArrayType(tpt, dims)
            }
            val name = Name(ctx.variableDeclaratorId.Identifier.getText)
            TreeFactories.mkValDef(Flags(PARAM), tpt, name, NoTree, pos(ctx))
          })
      }
    }

    override def visitTypeMethodHeader(ctx:
      Java1Parser.TypeMethodHeaderContext): Tree = {
      val mods       = modifiersTo(ctx.modifier, true)
      val tpt        = {
        val use  = visit(ctx.`type`()).asInstanceOf[UseTree]
        val dims = ctx.methodDeclarator.dims
        dimsToArrayType(use, dims)
      }
      val throwsClause = parseThrowsClause(ctx)
      val name       =
        Name(ctx.methodDeclarator.methodDeclaratorNoDims.Identifier.getText)
      val params     = formalParameterListToValDefs(
        ctx.methodDeclarator.methodDeclaratorNoDims.formalParameterList)
      TreeFactories.mkMethodDef(mods, tpt, name, params,
        throwsClause, NoTree, pos(ctx))
    }


    override def visitVoidMethodHeader(ctx:
      Java1Parser.VoidMethodHeaderContext): Tree = {
      val mods       = modifiersTo(ctx.modifier, true)
      val tpt        = TreeFactories.mkTypeUse(VOID_TYPE_NAME,
                              pos(ctx))
      val name       = Name(ctx.methodDeclaratorNoDims().Identifier.getText)
      val params     = formalParameterListToValDefs(
        ctx.methodDeclaratorNoDims().formalParameterList)
      val throwsClause = parseThrowsClause(ctx)
      TreeFactories.mkMethodDef(mods, tpt, name, params,
        throwsClause, NoTree, pos(ctx))
    }


    override def visitThrowsClause(ctx:
      Java1Parser.ThrowsClauseContext): Tree = {
      // INFO: Do not use this method
      visitChildren(ctx)
    }

    override def visitClassOrInterfaceTypeList(ctx:
      Java1Parser.ClassOrInterfaceTypeListContext): Tree = {
      // INFO: Do not use this method
      visitChildren(ctx)
    }

    override def visitMethodBody(ctx:
      Java1Parser.MethodBodyContext): Tree = {
      if(ctx.emptyStatement != null) visit(ctx.emptyStatement)
      else visit(ctx.block)
    }


    override def visitStaticInitializer(ctx:
      Java1Parser.StaticInitializerContext): Tree = {
      val res = visit(ctx.block).asInstanceOf[BlockApi]
      res.isStaticInit = true
      res
    }

    override def visitConstructorDeclaration(ctx:
      Java1Parser.ConstructorDeclarationContext): Tree = {
      val mods       = modifiersTo(ctx.modifier, true) | CONSTRUCTOR
      val declCtx    = ctx.constructorDeclarator.methodDeclaratorNoDims()
      val tpt        = {
        val id = declCtx.Identifier
        TreeFactories.mkTypeUse(Name(id.getText), pos(id.getSymbol))
      }
      val name       = CONSTRUCTOR_NAME
      val params     = formalParameterListToValDefs(declCtx.formalParameterList)
      val throwsClause = ctx.throwsClause match {
        case null                           => Nil
        case tc                             =>
          tc.classOrInterfaceTypeList
            .classOrInterfaceType.asScala.
            toList.map(visit(_).asInstanceOf[UseTree])
      }
      val body       = visit(ctx.constructorBody).asInstanceOf[Expr]
      TreeFactories.mkMethodDef(mods, tpt, name, params,
        throwsClause, body, pos(ctx))
    }


    override def visitConstructorDeclarator(ctx:
      Java1Parser.ConstructorDeclaratorContext): Tree = {
      visitChildren(ctx.methodDeclaratorNoDims)
    }


    override def visitConstructorBody(ctx:
      Java1Parser.ConstructorBodyContext): Tree = {
      val call  = ctx.explicitConstructorInvocation match {
        case null                           => Nil
        case ctx                            => List(visit(ctx))
      }
      val stmts = ctx.blockStatement match {
        case null                           => Nil
        case list                           =>
          list.asScala.toList.flatMap { (x) =>
            if(x.localVariableDeclarationStatement != null) {
              val ctx = x.localVariableDeclarationStatement
                         .localVariableDeclaration
              localVariableDeclaration(ctx)
            } else {
              List(visit(x.statement))
            }
          }
      }
      TreeFactories.mkBlock(call ++ stmts, pos(ctx))
    }

    override def visitExplicitConstructorInvocation(ctx:
      Java1Parser.ExplicitConstructorInvocationContext): Tree = {
      val args = ctx.argumentList match {
        case null                       => Nil
        case list                       =>
          list.expression.asScala.toList.map { (x) =>
            visit(x).asInstanceOf[Expr]
          }
      }
      val ps   = pos(ctx)
      val init = TreeFactories.mkIdent(CONSTRUCTOR_NAME,
        ps)
      init.isConstructorIdent = true
      val qual = ctx.qual.getText match {
        case "super"             =>
          TreeFactories.mkSuper(ps)
        case "this"              =>
          TreeFactories.mkThis(ps)
      }
      val fun  = TreeFactories.mkSelect(qual, init, ps)
      TreeFactories.mkApply(fun, args, ps)
    }


    override def visitInterfaceDeclaration(ctx:
      Java1Parser.InterfaceDeclarationContext): Tree = {
      val mods       = modifiersTo(ctx.modifier, true) | INTERFACE
      val name       = Name(ctx.Identifier.getText)
      val interfaces = interfacesContextToTypeUses(
        ctx.extendsInterfaces())
      interfaces.foreach { t => t match {
          case tuse: TypeUseApi                =>
            tuse.isInImplementsClause = true
          case Select(_, tuse: TypeUseApi)     =>
            tuse.isInImplementsClause = true
          case _                               =>
            ()
        }
      }
      val body       =
        visit(ctx.interfaceBody()).asInstanceOf[TemplateApi]
      val res = TreeFactories.mkClassDef(mods,
        name, interfaces, body, pos(ctx))
      if(mods.isPublicAcc)
        res.sourceName = fileName
      res

    }

    override def visitExtendsInterfaces(ctx:
      Java1Parser.ExtendsInterfacesContext): Tree = {
      // INFO: Do not use this method
      visitChildren(ctx)
    }

    override def visitInterfaceBody(ctx:
      Java1Parser.InterfaceBodyContext): Tree = {
      ctx.interfaceMemberDeclaration() match {
        case null                     =>
          TreeFactories.mkTemplate(Nil, pos(ctx))
        case body                     =>
          val members: List[Tree] = body.asScala.toList.flatMap { x =>
            if(x.constantDeclaration != null) {
              val ctx        = x.constantDeclaration.fieldDeclaration
              val mods       =
                modifiersTo(ctx.modifier, true) | FIELD | FINAL | STATIC
              val tpt        = visit(ctx.`type`()).asInstanceOf[UseTree]
              ctx.variableDeclarators.variableDeclarator.asScala.toList.map {
                (ctx) => {
                  val tpt2   =
                    dimsToArrayType(tpt, ctx.variableDeclaratorId.dims)
                  val name   = Name(ctx.variableDeclaratorId.Identifier.getText)
                  val rhs    = ctx.variableInitializer match {
                    case null          => NoTree
                    case child         => visit(child).asInstanceOf[Expr]
                  }
                  TreeFactories.mkValDef(mods, tpt2, name, rhs, pos(ctx))
                }
              }
            } else {
              List(visit(x))
            }
          }
          TreeFactories.mkTemplate(members, pos(ctx))
      }
    }

    override def visitInterfaceMemberDeclaration(ctx:
      Java1Parser.InterfaceMemberDeclarationContext): Tree = {
      // INFO: Do not use this method
      visitChildren(ctx)
    }

    override def visitConstantDeclaration(ctx:
      Java1Parser.ConstantDeclarationContext): Tree = {
      visit(ctx.fieldDeclaration) match {
        case vd: ValDefApi             =>
          TreeFactories.mkValDef(vd.mods | FINAL,
            vd.tpt, vd.name, vd.rhs, vd.pos)
        case t                         =>
          t
      }
    }

    override def visitAbstractMethodDeclaration(ctx:
      Java1Parser.AbstractMethodDeclarationContext): Tree = {
      val mthd = if(ctx.typeMethodHeader == null)
        visit(ctx.voidMethodHeader)
      else visit(ctx.typeMethodHeader)
      mthd match {
        case md: MethodDefApi          =>
          TreeFactories.mkMethodDef(md.mods | ABSTRACT,
            md.ret, md.name, md.params, md.throwsClause, md.body,
            md.pos)
        case t                         =>
          t
      }
    }


    override def visitArrayInitializer(ctx:
      Java1Parser.ArrayInitializerContext): Tree = {
      val exprs = if(ctx.variableInitializers == null) Nil
      else {
        val varsCntx = ctx.variableInitializers.variableInitializer
        if(varsCntx == null)
          Nil
        else
          varsCntx.asScala.toList.map(visit(_).asInstanceOf[Expr])
      }
      TreeFactories.mkArrayInitializer(exprs, pos(ctx))
    }


    override def visitVariableInitializers(ctx:
      Java1Parser.VariableInitializersContext): Tree = {
      // INFO: Do not use this method
      visitChildren(ctx)
    }

    override def visitBlock(ctx:
      Java1Parser.BlockContext): Tree = {
      val stmts = ctx.blockStatement match {
        case null                           => Nil
        case list                           =>
          list.asScala.toList.flatMap { (x) =>
            if(x.localVariableDeclarationStatement != null) {
              val ctx = x.localVariableDeclarationStatement
                         .localVariableDeclaration
              localVariableDeclaration(ctx)
            } else {
              List(visit(x.statement))
            }
          }
      }
      TreeFactories.mkBlock(stmts, pos(ctx))
    }

    override def visitBlockStatement(ctx:
      Java1Parser.BlockStatementContext): Tree = {
      // INFO: Do not use this method
      visitChildren(ctx)
    }

    override def visitLocalVariableDeclarationStatement(ctx:
      Java1Parser.LocalVariableDeclarationStatementContext): Tree = {
      // INFO: Do not use this method
      visit(ctx.localVariableDeclaration)
    }

    override def visitLocalVariableDeclaration(ctx:
      Java1Parser.LocalVariableDeclarationContext): Tree = {
      // INFO: Do not use this method
      visitChildren(ctx)
    }

    override def visitStatement(ctx:
      Java1Parser.StatementContext): Tree = {
      // INFO: Do not use this method
      visitChildren(ctx)
    }

    override def visitStatementNoShortIf(ctx:
      Java1Parser.StatementNoShortIfContext): Tree = {
      // INFO: Do not use this method
      visitChildren(ctx)
    }

    override def visitStatementWithoutTrailingSubstatement(ctx:
      Java1Parser.StatementWithoutTrailingSubstatementContext): Tree = {
      // INFO: Do not use this method
      visitChildren(ctx)
    }

    override def visitEmptyStatement(ctx:
      Java1Parser.EmptyStatementContext): Tree = {
      NoTree
    }

    override def visitLabeledStatement(ctx:
      Java1Parser.LabeledStatementContext): Tree = {
      val id   = Name(ctx.Identifier.getText)
      val expr = visit(ctx.statement).asInstanceOf[Expr]
      TreeFactories.mkLabel(id, expr, pos(ctx))
    }

    override def visitLabeledStatementNoShortIf(ctx:
      Java1Parser.LabeledStatementNoShortIfContext): Tree = {
      val id   = Name(ctx.Identifier.getText)
      val expr = visit(ctx.statementNoShortIf).asInstanceOf[Expr]
      TreeFactories.mkLabel(id, expr, pos(ctx))
    }

    override def visitExpressionStatement(ctx:
      Java1Parser.ExpressionStatementContext): Tree = {
      visit(ctx.statementExpression)
    }

    override def visitStatementExpression(ctx:
      Java1Parser.StatementExpressionContext): Tree = {
      // INFO: Do not use this method
      visitChildren(ctx)
    }

    override def visitIfThenStatement(ctx:
      Java1Parser.IfThenStatementContext): Tree = {
      val cond    = visit(ctx.expression).asInstanceOf[Expr]
      val thenp   = visit(ctx.statement).asInstanceOf[Expr]
      val elsep   = NoTree
      TreeFactories.mkIf(cond, thenp, elsep, pos(ctx))
    }

    override def visitIfThenElseStatement(ctx:
      Java1Parser.IfThenElseStatementContext): Tree = {
      val cond    = visit(ctx.expression).asInstanceOf[Expr]
      val thenp   = visit(ctx.statementNoShortIf).asInstanceOf[Expr]
      val elsep   = visit(ctx.statement).asInstanceOf[Expr]
      TreeFactories.mkIf(cond, thenp, elsep, pos(ctx))
    }

    override def visitIfThenElseStatementNoShortIf(ctx:
      Java1Parser.IfThenElseStatementNoShortIfContext): Tree = {
      val cond    = visit(ctx.expression).asInstanceOf[Expr]
      // first branch
      val thenp   = visit(ctx.statementNoShortIf.get(0)).asInstanceOf[Expr]
      // second branch
      val elsep   = visit(ctx.statementNoShortIf.get(1)).asInstanceOf[Expr]
      TreeFactories.mkIf(cond, thenp, elsep, pos(ctx))
    }

    override def visitSwitchStatement(ctx:
      Java1Parser.SwitchStatementContext): Tree = {
      val expr      = visit(ctx.expression).asInstanceOf[Expr]
      val bodyCtx   = ctx.switchBlock
      val body      = {
        val cases1  = bodyCtx.switchBlockStatementGroups match {
          case null                           => Nil
          case groups                         =>
            groups.switchBlockStatementGroup.asScala.toList.map { (x) =>
              visit(x).asInstanceOf[Case]
            }
        }
        val cases2  = bodyCtx.switchLabel match {
          case null                           => Nil
          case labels                         =>
            val lbls = labels.asScala.toList.flatMap { (ctx) =>
              if(ctx.defaultCase == null) {
                List(visit(ctx.caseLabel.constantExpression).asInstanceOf[Expr])
              } else Nil
            }
            lbls match {
              case Nil =>
                Nil
              case _   =>
                val block = TreeFactories.mkBlock(Nil, pos(bodyCtx))
                List(TreeFactories.mkCase(lbls, block, pos(bodyCtx)))
            }
        }
        cases1 ++ cases2
      }
      TreeFactories.mkSwitch(expr, body, pos(ctx))
    }

    override def visitSwitchBlock(ctx: Java1Parser.SwitchBlockContext): Tree = {
      // INFO: Do not use this method
      visitChildren(ctx)
    }

    override def visitSwitchBlockStatementGroups(ctx:
      Java1Parser.SwitchBlockStatementGroupsContext): Tree = {
      // INFO: Do not use this method
      visitChildren(ctx)
    }

    override def visitSwitchBlockStatementGroup(ctx:
      Java1Parser.SwitchBlockStatementGroupContext): Tree = {
      // INFO: Do not use this method
      val guards = ctx.switchLabel.asScala.toList.flatMap { (ctx) =>
        if(ctx.defaultCase == null) {
          List(visit(ctx.caseLabel.constantExpression).asInstanceOf[Expr])
        } else Nil
      }
      val stmts  = ctx.blockStatement match {
        case null                           => Nil
        case list                           =>
          list.asScala.toList.flatMap { (x) =>
            if(x.localVariableDeclarationStatement != null) {
              val ctx = x.localVariableDeclarationStatement
                         .localVariableDeclaration
              localVariableDeclaration(ctx)
            } else {
              List(visit(x.statement))
            }
          }
      }
      val body = TreeFactories.mkBlock(stmts, pos(ctx))
      TreeFactories.mkCase(guards, body, pos(ctx))
    }


    override def visitSwitchLabel(ctx:
      Java1Parser.SwitchLabelContext): Tree = {
      // INFO: Do not use this method
      visitChildren(ctx)
    }

    override def visitCaseLabel(ctx:
      Java1Parser.CaseLabelContext): Tree = {
      // INFO: Do not use this method
      visitChildren(ctx)
    }

    override def visitDefaultCase(ctx:
      Java1Parser.DefaultCaseContext): Tree = {
      // INFO: Do not use this method
      visitChildren(ctx)
    }

    override def visitWhileStatement(ctx:
      Java1Parser.WhileStatementContext): Tree = {
      val expr      = visit(ctx.expression).asInstanceOf[Expr]
      val body      = visit(ctx.statement).asInstanceOf[Expr]
      TreeFactories.mkWhile(false, expr, body, pos(ctx))
    }

    override def visitWhileStatementNoShortIf(ctx:
      Java1Parser.WhileStatementNoShortIfContext): Tree = {
      val expr      = visit(ctx.expression).asInstanceOf[Expr]
      val body      = visit(ctx.statementNoShortIf).asInstanceOf[Expr]
      TreeFactories.mkWhile(false, expr, body, pos(ctx))
    }

    override def visitDoStatement(ctx:
      Java1Parser.DoStatementContext): Tree = {
      val expr      = visit(ctx.expression).asInstanceOf[Expr]
      val body      = visit(ctx.statement).asInstanceOf[Expr]
      TreeFactories.mkWhile(true, expr, body, pos(ctx))
    }


    override def visitForStatement(ctx:
      Java1Parser.ForStatementContext): Tree = {
      val inits    = ctx.forInit match {
        case null                                            => Nil
        case init   if init.statementExpressionList == null  =>
          localVariableDeclaration(init.localVariableDeclaration)
        case inits                                           =>
          inits
            .statementExpressionList
            .statementExpression
            .asScala.toList.map {(x) =>
              visit(x).asInstanceOf[Expr]
            }
      }
      val steps    = ctx.forUpdate match {
        case null                                            => Nil
        case steps                                           =>
          steps
            .statementExpressionList
            .statementExpression
            .asScala.toList.map {(x) => visit(x).asInstanceOf[Expr]}
      }
      val cond     = ctx.expression match {
        case null                          =>
          TreeFactories.mkLiteral(BooleanConstant(true), pos(ctx))
        case expr                          => visit(expr).asInstanceOf[Expr]
      }
      val body     = visit(ctx.statement).asInstanceOf[Expr]
      TreeFactories.mkFor(inits, cond, steps, body, pos(ctx))
    }

    override def visitForStatementNoShortIf(ctx:
      Java1Parser.ForStatementNoShortIfContext): Tree = {
      val inits    = ctx.forInit match {
        case null                                            => Nil
        case init   if init.statementExpressionList == null  =>
          localVariableDeclaration(init.localVariableDeclaration)
        case inits                                           =>
          inits
            .statementExpressionList
            .statementExpression
            .asScala.toList.map {(x) =>
              visit(x).asInstanceOf[Expr]
            }
      }
      val steps    = ctx.forUpdate match {
        case null                                            => Nil
        case steps                                           =>
          steps
            .statementExpressionList
            .statementExpression
            .asScala.toList.map {(x) => visit(x).asInstanceOf[Expr]}
      }
      val cond     = ctx.expression match {
        case null                          =>
          TreeFactories.mkLiteral(BooleanConstant(true), pos(ctx))
        case expr                          => visit(expr).asInstanceOf[Expr]
      }
      val body     = visit(ctx.statementNoShortIf).asInstanceOf[Expr]
      TreeFactories.mkFor(inits, cond, steps, body, pos(ctx))
    }

    override def visitForInit(ctx:
      Java1Parser.ForInitContext): Tree = {
      // INFO: Do not implement this
      visitChildren(ctx)
    }

    override def visitForUpdate(ctx:
      Java1Parser.ForUpdateContext): Tree = {
      // INFO: Do not implement this
      visitChildren(ctx)
    }

    override def visitStatementExpressionList(ctx:
      Java1Parser.StatementExpressionListContext): Tree = {
      // INFO: Do not implement this
      visitChildren(ctx)
    }

    override def visitBreakStatement(ctx:
      Java1Parser.BreakStatementContext): Tree = {
      val id      = ctx.Identifier match {
        case null                        => None
        case name                        => Some(Name(name.getText))
      }
      TreeFactories.mkBreak(id, pos(ctx))
    }

    override def visitContinueStatement(ctx:
      Java1Parser.ContinueStatementContext): Tree = {
      val id      = ctx.Identifier match {
        case null                        => None
        case name                        => Some(Name(name.getText))
      }
      TreeFactories.mkContinue(id, pos(ctx))
    }

    override def visitReturnStatement(ctx:
      Java1Parser.ReturnStatementContext): Tree = {
      ctx.expression match {
        case null                        =>
          TreeFactories.mkReturn(None, pos(ctx))
        case expr                        =>
          val e = visit(expr).asInstanceOf[Expr]
          TreeFactories.mkReturn(Some(e), pos(ctx))
      }
    }

    override def visitThrowStatement(ctx:
      Java1Parser.ThrowStatementContext): Tree = {
      val expr = visit(ctx.expression).asInstanceOf[Expr]
      TreeFactories.mkThrow(expr, pos(ctx))
    }

    override def visitSynchronizedStatement(ctx:
      Java1Parser.SynchronizedStatementContext): Tree = {
      val expr = visit(ctx.expression).asInstanceOf[Expr]
      val block = visit(ctx.block).asInstanceOf[BlockApi]
      TreeFactories.mkSynchronized(expr, block, pos(ctx))
    }

    override def visitTryStatement(ctx:
      Java1Parser.TryStatementContext): Tree = {
      val tryClause = visit(ctx.block).asInstanceOf[BlockApi]
      val catches   = ctx.catches match {
        case null      => Nil
        case cs        =>
          cs.catchClause.asScala.toList.map(visit(_).asInstanceOf[CatchApi])
      }
      val finallyClause = ctx.finallyClause match {
        case null      => None
        case t         => Some(visit(t).asInstanceOf[BlockApi])
      }
      TreeFactories.mkTry(tryClause, catches, finallyClause, pos(ctx))
    }

    override def visitCatches(ctx:
      Java1Parser.CatchesContext): Tree = {
      // INFO: Do not implement this
      visitChildren(ctx)
    }

    override def visitCatchClause(ctx:
      Java1Parser.CatchClauseContext): Tree = {
      val param = {
        val tpt  = {
          val tpt = visit(ctx.formalParameter.`type`()).asInstanceOf[UseTree]
          val dims = ctx.formalParameter.variableDeclaratorId.dims
          dimsToArrayType(tpt, dims)
        }
        val name = Name(ctx.formalParameter
          .variableDeclaratorId.Identifier.getText)
        TreeFactories.mkValDef(PARAM | EXCEPTION_PARAM,
          tpt, name, NoTree, pos(ctx))
      }
      val block = visit(ctx.block).asInstanceOf[BlockApi]
      TreeFactories.mkCatch(param, block, pos(ctx))
    }

    override def visitFinallyClause(ctx:
      Java1Parser.FinallyClauseContext): Tree = {
      // INFO: Do not implement this
      visitChildren(ctx)
    }

    override def visitPrimary(ctx:
      Java1Parser.PrimaryContext): Tree = {
      // INFO: Do not implement this
      visitChildren(ctx)
    }

    override def visitArrayAccess(ctx:
      Java1Parser.ArrayAccessContext): Tree = {
      val array = if(ctx.name != null) visit(ctx.name).asInstanceOf[Expr]
                  else visit(ctx.primaryNoNewArray).asInstanceOf[Expr]
      val index = visit(ctx.expression).asInstanceOf[Expr]
      TreeFactories.mkArrayAccess(array, index, pos(ctx))
    }

    override def visitPrimaryLit(ctx:
      Java1Parser.PrimaryLitContext): Tree = {
      // INFO: Do not implement this
      visitChildren(ctx)
    }

    override def visitPrimaryThis(ctx:
      Java1Parser.PrimaryThisContext): Tree = {
      TreeFactories.mkThis(pos(ctx))
    }

    override def visitPrimaryExpr(ctx:
      Java1Parser.PrimaryExprContext): Tree = {
      // INFO: Do not implement this
      visit(ctx.expression)
    }

    override def visitPrimaryNew(ctx:
      Java1Parser.PrimaryNewContext): Tree = {
      // INFO: Do not implement this
      visitChildren(ctx)
    }

    override def visitPrimaryNewArraySelect(ctx:
      Java1Parser.PrimaryNewArraySelectContext): Tree = {
      val qual = visit(ctx.arrayCreationExpression)
      val id   = TreeFactories.mkIdent(Name(ctx.Identifier.getText),
                        pos(ctx.Identifier.getSymbol))
      TreeFactories.mkSelect(qual, id, pos(ctx))
    }

    override def visitPrimarySelect(ctx:
      Java1Parser.PrimarySelectContext): Tree = {
      val id   = TreeFactories.mkIdent(Name(ctx.Identifier.getText),
                        pos(ctx.Identifier.getSymbol))
      val qual = visit(ctx.primaryNoNewArray)
      TreeFactories.mkSelect(qual, id, pos(ctx))
    }

    override def visitPrimarySuperSelect(ctx:
      Java1Parser.PrimarySuperSelectContext): Tree = {
      val id   = TreeFactories.mkIdent(Name(ctx.Identifier.getText),
                        pos(ctx.Identifier.getSymbol))
      val qual = TreeFactories.mkSuper(pos(ctx))
      TreeFactories.mkSelect(qual, id, pos(ctx))
    }

    override def visitPrimaryApply(ctx:
      Java1Parser.PrimaryApplyContext): Tree = {
      val fun = visit(ctx.name).asInstanceOf[Expr]
      val args = ctx.argumentList match {
        case null                       => Nil
        case list                       =>
          list.expression.asScala.toList.map { (x) =>
            visit(x).asInstanceOf[Expr]
          }
      }
      TreeFactories.mkApply(fun, args, pos(ctx))
    }

    override def visitPrimaryQualApply(ctx:
      Java1Parser.PrimaryQualApplyContext): Tree = {
      val ps   = pos(ctx)
      val id   = TreeFactories.mkIdent(Name(ctx.Identifier.getText),
                        pos(ctx.Identifier.getSymbol))
      val qual = visit(ctx.primaryNoNewArray)
      val fun  = TreeFactories.mkSelect(qual, id, pos(ctx))
      val args = ctx.argumentList match {
        case null                       => Nil
        case list                       =>
          list.expression.asScala.toList.map { (x) =>
            visit(x).asInstanceOf[Expr]
          }
      }
      TreeFactories.mkApply(fun, args, ps)
    }

    override def visitPrimaryArrayApply(ctx:
      Java1Parser.PrimaryArrayApplyContext): Tree = {
      val ps   = pos(ctx)
      val id   = TreeFactories.mkIdent(Name(ctx.Identifier.getText),
                        pos(ctx.Identifier.getSymbol))
      val qual = visit(ctx.arrayCreationExpression)
      val fun  = TreeFactories.mkSelect(qual, id, pos(ctx))
      val args = ctx.argumentList match {
        case null                       => Nil
        case list                       =>
          list.expression.asScala.toList.map { (x) =>
            visit(x).asInstanceOf[Expr]
          }
      }
      TreeFactories.mkApply(fun, args, ps)
    }

    override def visitPrimarySuperApply(ctx:
      Java1Parser.PrimarySuperApplyContext): Tree = {
      val ps   = pos(ctx)
      val id   = TreeFactories.mkIdent(Name(ctx.Identifier.getText),
                        pos(ctx.Identifier.getSymbol))
      val qual = TreeFactories.mkSuper(ps)
      val fun  = TreeFactories.mkSelect(qual, id, pos(ctx))
      val args = ctx.argumentList match {
        case null                       => Nil
        case list                       =>
          list.expression.asScala.toList.map { (x) =>
            visit(x).asInstanceOf[Expr]
          }
      }
      TreeFactories.mkApply(fun, args, ps)
    }

    override def visitPrimaryArrayAccess(ctx:
      Java1Parser.PrimaryArrayAccessContext): Tree = {
      val array = visit(ctx.name).asInstanceOf[Expr]
      val index = visit(ctx.expression).asInstanceOf[Expr]
      TreeFactories.mkArrayAccess(array, index, pos(ctx))
    }

    override def visitPrimaryArrayAccess2(ctx:
      Java1Parser.PrimaryArrayAccess2Context): Tree = {
      val array = visit(ctx.primaryNoNewArray).asInstanceOf[Expr]
      val index = visit(ctx.expression).asInstanceOf[Expr]
      TreeFactories.mkArrayAccess(array, index, pos(ctx))
    }


    override def visitClassInstanceCreationExpression(ctx:
      Java1Parser.ClassInstanceCreationExpressionContext): Tree = {
      val qual    = visit(ctx.classOrInterfaceType)
      val ps      = pos(ctx)
      val init    = TreeFactories.mkIdent(CONSTRUCTOR_NAME, ps)
      init.isConstructorIdent = true
      val fun     = TreeFactories.mkSelect(qual, init, ps)
      val args = ctx.argumentList match {
        case null                       => Nil
        case list                       =>
          list.expression.asScala.toList.map { (x) =>
            visit(x).asInstanceOf[Expr]
          }
      }
      val app = TreeFactories.mkApply(fun, args, ps)
      TreeFactories.mkNew(app, ps)
    }

    override def visitArgumentList(ctx:
      Java1Parser.ArgumentListContext): Tree = {
      // INFO: Do not implement this
      visitChildren(ctx)
    }

    override def visitPrimitiveArrayCreation(ctx:
      Java1Parser.PrimitiveArrayCreationContext): Tree = {
      val ebase = visit(ctx.primitiveType).asInstanceOf[Expr]
      val edims = if(ctx.dims != null) {
        ctx.dims.dim.asScala.foldLeft(ebase)((z, y) => {
          TreeFactories.mkArrayCreation(z, None, pos(y))
        })
      } else {
        ebase
      }
      ctx.dimExpr.asScala.reverse.foldLeft(edims)((z, y) => {
        val d = visit(y).asInstanceOf[Expr]
        TreeFactories.mkArrayCreation(z, Some(d), pos(y))
      })
    }

    override def visitReferenceArrayCreation(ctx:
      Java1Parser.ReferenceArrayCreationContext): Tree = {
      val ebase = visit(ctx.classOrInterfaceType).asInstanceOf[Expr]
      val edims = if(ctx.dims != null) {
        ctx.dims.dim.asScala.foldLeft(ebase)((z, y) => {
          TreeFactories.mkArrayCreation(z, None, pos(y))
        })
      } else {
        ebase
      }
      ctx.dimExpr.asScala.reverse.foldLeft(edims)((z, y) => {
        val d = visit(y).asInstanceOf[Expr]
        TreeFactories.mkArrayCreation(z, Some(d), pos(y))
      })
    }

    override def visitDimExpr(ctx:
      Java1Parser.DimExprContext): Tree = {
      visit(ctx.expression)
    }

    override def visitDims(ctx:
      Java1Parser.DimsContext): Tree = {
      // INFO: Do not implement this
      visitChildren(ctx)
    }

    override def visitQualifiedFieldAccess(ctx:
      Java1Parser.QualifiedFieldAccessContext): Tree = {
      val id   = TreeFactories.mkIdent(Name(ctx.Identifier.getText),
                        pos(ctx.Identifier.getSymbol))
      val qual = visit(ctx.primary)
      TreeFactories.mkSelect(qual, id, pos(ctx))
    }

    override def visitSuperFieldAccess(ctx:
      Java1Parser.SuperFieldAccessContext): Tree = {
      val id   = TreeFactories.mkIdent(Name(ctx.Identifier.getText),
                        pos(ctx.Identifier.getSymbol))
      val qual = TreeFactories.mkSuper(pos(ctx))
      TreeFactories.mkSelect(qual, id, pos(ctx))
    }

    override def visitSimpleMethodInvocation(ctx:
      Java1Parser.SimpleMethodInvocationContext): Tree = {
      val ps   = pos(ctx)
      val fun = visit(ctx.name).asInstanceOf[Expr]
      val args = ctx.argumentList match {
        case null                       => Nil
        case list                       =>
          list.expression.asScala.toList.map { (x) =>
            visit(x).asInstanceOf[Expr]
          }
      }
      TreeFactories.mkApply(fun, args, ps)
    }

    override def visitQualifiedMethodInvocation(ctx:
      Java1Parser.QualifiedMethodInvocationContext): Tree = {
      val ps   = pos(ctx)
      val id   = TreeFactories.mkIdent(Name(ctx.Identifier.getText),
                        pos(ctx.Identifier.getSymbol))
      val qual = visit(ctx.primary)
      val fun  = TreeFactories.mkSelect(qual, id, pos(ctx))
      val args = ctx.argumentList match {
        case null                       => Nil
        case list                       =>
          list.expression.asScala.toList.map { (x) =>
            visit(x).asInstanceOf[Expr]
          }
      }
      TreeFactories.mkApply(fun, args, ps)
    }

    override def visitSuperMethodInvocation(ctx:
      Java1Parser.SuperMethodInvocationContext): Tree = {
      val ps   = pos(ctx)
      val id   = TreeFactories.mkIdent(Name(ctx.Identifier.getText),
                        pos(ctx.Identifier.getSymbol))
      val qual = TreeFactories.mkSuper(pos(ctx))
      val fun  = TreeFactories.mkSelect(qual, id, pos(ctx))
      val args = ctx.argumentList match {
        case null                       => Nil
        case list                       =>
          list.expression.asScala.toList.map { (x) =>
            visit(x).asInstanceOf[Expr]
          }
      }
      TreeFactories.mkApply(fun, args, ps)
    }

    override def visitPrimaryExpression(ctx:
      Java1Parser.PrimaryExpressionContext): Tree = {
      // INFO: Do not implement this
      visitChildren(ctx)
    }

    override def visitNameExpression(ctx:
      Java1Parser.NameExpressionContext): Tree = {
      // INFO: Do not implement this
      visitChildren(ctx)
    }

    override def visitCastExpression(ctx:
      Java1Parser.CastExpressionContext): Tree = {
      // INFO: Do not implement this
      val tpt  = visit(ctx.`type`()).asInstanceOf[UseTree]
      val expr = visit(ctx.expression).asInstanceOf[Expr]
      TreeFactories.mkCast(tpt, expr, pos(ctx))
    }

    override def visitPostfixExpression(ctx:
      Java1Parser.PostfixExpressionContext): Tree = {
      createUnaryOrPostfix(true, ctx.expression, ctx.op.getText, ctx)
    }

    override def visitUnaryExpression(ctx:
      Java1Parser.UnaryExpressionContext): Tree = {
      createUnaryOrPostfix(false, ctx.expression, ctx.op.getText, ctx)
    }

    override def visitBitwiseUnaryExpression(ctx:
      Java1Parser.BitwiseUnaryExpressionContext): Tree = {
      createUnaryOrPostfix(false, ctx.expression, ctx.op.getText, ctx)
    }


    override def visitAssignment(ctx:
      Java1Parser.AssignmentContext): Tree = {
      val lhs      = visit(ctx.leftHandSide).asInstanceOf[Expr]
      val rhs      = visit(ctx.expression).asInstanceOf[Expr]
      TreeFactories.mkAssign(lhs, rhs, pos(ctx))
    }

    override def visitLeftHandSide(ctx:
      Java1Parser.LeftHandSideContext): Tree = {
      // INFO: Do not implement this
      visitChildren(ctx)
    }

    override def visitMulBinaryExpression(ctx:
      Java1Parser.MulBinaryExpressionContext): Tree = {
      createBinary(ctx.expression, ctx.op.getText, ctx)
    }

    override def visitAddBinaryExpression(ctx:
      Java1Parser.AddBinaryExpressionContext): Tree = {
      createBinary(ctx.expression, ctx.op.getText, ctx)
    }

    override def visitShiftBinaryExpression(ctx:
      Java1Parser.ShiftBinaryExpressionContext): Tree = {
      createBinary(ctx.expression, ctx.op.getText, ctx)
    }

    override def visitRelBinaryExpression(ctx:
      Java1Parser.RelBinaryExpressionContext): Tree = {
      createBinary(ctx.expression, ctx.op.getText, ctx)
    }

    override def visitInstanceOfExpression(ctx:
      Java1Parser.InstanceOfExpressionContext): Tree = {
      val expr = visit(ctx.expression).asInstanceOf[Expr]
      val tpt  = visit(ctx.`type`).asInstanceOf[UseTree]
      TreeFactories.mkBinary(expr, InstanceOf, tpt, pos(ctx))
    }

    override def visitEquBinaryExpression(ctx:
      Java1Parser.EquBinaryExpressionContext): Tree = {
      createBinary(ctx.expression, ctx.op.getText, ctx)
    }

    override def visitBitAndBinaryExpression(ctx:
      Java1Parser.BitAndBinaryExpressionContext): Tree = {
      createBinary(ctx.expression, "&", ctx)
    }

    override def visitBitOrBinaryExpression(ctx:
      Java1Parser.BitOrBinaryExpressionContext): Tree = {
      createBinary(ctx.expression, "|", ctx)
    }

    override def visitBitXOrBinaryExpression(ctx:
      Java1Parser.BitXOrBinaryExpressionContext): Tree = {
      createBinary(ctx.expression, "^", ctx)
    }

    override def visitAndBinaryExpression(ctx:
      Java1Parser.AndBinaryExpressionContext): Tree = {
      createBinary(ctx.expression, "&&", ctx)
    }

    override def visitOrBinaryExpression(ctx:
      Java1Parser.OrBinaryExpressionContext): Tree = {
      createBinary(ctx.expression, "||", ctx)
    }

    override def visitAssignExpression(ctx:
      Java1Parser.AssignExpressionContext): Tree = {
      val lhs      = visit(ctx.leftHandSide).asInstanceOf[Expr]
      val rhs      = visit(ctx.expression).asInstanceOf[Expr]
      ctx.op.getText match {
        case "="                         =>
          TreeFactories.mkAssign(lhs, rhs, pos(ctx))
        case op                          =>
          val op2      = op.take(op.length - 1)
          val expr     = createBinary(lhs, rhs, op2, ctx)
          expr.isCompoundBinary = true
          TreeFactories.mkAssign(lhs, expr, pos(ctx))
      }
    }

    override def visitTernaryExpression(ctx:
      Java1Parser.TernaryExpressionContext): Tree = {
      val cond   = visit(ctx.expression.get(0)).asInstanceOf[Expr]
      val thenp  = visit(ctx.expression.get(1)).asInstanceOf[Expr]
      val elsep  = visit(ctx.expression.get(2)).asInstanceOf[Expr]
      TreeFactories.mkTernary(cond, thenp, elsep, pos(ctx))
    }

    override def visitConstantExpression(ctx:
      Java1Parser.ConstantExpressionContext): Tree = {
      // INFO: Do not implement this
      visitChildren(ctx)
    }
  }
}


object Parser extends Parser
