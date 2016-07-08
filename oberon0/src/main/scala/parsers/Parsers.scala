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

package ch.usi.inf.l3.sana.oberon0.parsers

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.ooj
import sana.arrayj
import sana.brokenj
import sana.oberon0
import tiny.source.{SourceFile, Position}
import tiny.symbols._
import tiny.ast.{TreeCopiers => _, TreeFactories => _, _}
import tiny.names.Name
import oberon0.names.StdNames._
import tiny.modifiers.Flags
import tiny.parsers
import tiny.errors.ErrorReporting.error
import tiny.debug.logger
import calcj.ast.{TreeCopiers => _, TreeFactories => _, _}
import calcj.ast.operators._
import primj.ast.{TreeCopiers => _, TreeFactories => _, ProgramApi => _, _}
import brokenj.ast.{TreeCopiers => _, TreeFactories => _, _}
import ooj.ast.{TreeCopiers => _, TreeFactories => _, _}
import arrayj.ast.{TreeCopiers => _, TreeFactories => _, _}
import oberon0.ast._
import oberon0.errors.ErrorCodes._
import oberon0.ast.Implicits._
import oberon0.antlr._
import ooj.modifiers._
import primj.modifiers._
import ooj.modifiers.Ops._



import org.antlr.v4.runtime.misc.NotNull
import org.antlr.v4.runtime.{Token, ParserRuleContext}
import org.antlr.v4.runtime.tree.{AbstractParseTreeVisitor, TerminalNode}


import scala.collection.JavaConverters._

class Parser extends parsers.Parser {


  def parse(source: SourceFile): Tree = {
    val tree = new Oberon0Visitor(source.name,
      source.lines).visit(source.content)
    logger.debug(tree.toString)
    tree
  }

  class Oberon0Visitor(val source: String,
    lines: Array[String]) extends Oberon0BaseVisitor[Tree] {

    private[this] var seqNum = 0

    def pos(ctx: ParserRuleContext): Option[Position] = {
      val token = ctx.getStart
      pos(token)
    }

    def pos(ctx: TerminalNode): Option[Position] = {
      val token = ctx.getSymbol
      pos(token)
    }

    def pos(token: Token): Option[Position] = {
      Some(Position(source, lines,
        token.getLine, token.getCharPositionInLine + 1))
    }



    def nextTypeName(): Name = this.synchronized {
      val res = Name(NEW_TYPE_NAME_BASE + seqNum.toString)
      seqNum = seqNum + 1
      res
    }

    override def visitName(ctx: Oberon0Parser.NameContext): Tree = {
      // INFO: No need to implement this
      visitChildren(ctx)
    }

    override def visitSelector(ctx: Oberon0Parser.SelectorContext): Tree = {
      // INFO: No need to implement this
      visitChildren(ctx)
    }

    override def visitNumber(ctx: Oberon0Parser.NumberContext): Tree = {
      val value = ctx.Integer.getText.toInt
      val const = IntConstant(value)
      TreeFactories.mkLiteral(const, pos(ctx.Integer))
    }

    override def visitBooleanValue(
      ctx: Oberon0Parser.BooleanValueContext): Tree = {
      val value = ctx.value.getText match {
        case "TRUE"           => true
        case "FALSE"          => false
      }
      val const = BooleanConstant(value)
      TreeFactories.mkLiteral(const, pos(ctx.value))
    }

    override def visitFactor(ctx: Oberon0Parser.FactorContext): Tree = {
      if(ctx.select != null) visit(ctx.select)
      else if(ctx.number != null) visit(ctx.number)
      else if(ctx.booleanValue != null) visit(ctx.booleanValue)
      else if(ctx.expression != null) visit(ctx.expression)
      else { // if(ctx.factor != null)
        val expr = visit(ctx.factor).asInstanceOf[Expr]
        TreeFactories.mkUnary(false, Not, expr, pos(ctx))
      }
    }

    override def visitTerm2(ctx: Oberon0Parser.Term2Context): Tree = {
      // INFO: No need to implement this
      visitChildren(ctx)
    }

    override def visitTerm(ctx: Oberon0Parser.TermContext): Tree = {
      val lhs = visit(ctx.factor).asInstanceOf[Expr]
      val rhs = {
        if(ctx.term2 == null) None
        else {
          val rhss = ctx.term2.asScala.toList.reverse
          val z: Option[Expr => BinaryApi] = None
          rhss.foldLeft(z)((z, y) => {
            val op = y.op.getText match {
              case "*"               => Mul
              case "DIV"             => Div
              case "MOD"             => Mod
              case "AND"             => And
            }
            val e1 = visit(y.factor).asInstanceOf[Expr]
            z match {
              case Some(f)       =>
                val rhs = f(e1)
                Some {
                  (lhs: Expr) =>
                    TreeFactories.mkBinary(lhs, op, rhs, pos(y.factor))
                }
              case None          =>
                Some {
                  (lhs: Expr) =>
                    TreeFactories.mkBinary(lhs, op, e1, pos(y.factor))
                }
            }
          })
        }
      }
      rhs.map(f => f(lhs)).getOrElse(lhs)
    }

    override def visitSelect(ctx: Oberon0Parser.SelectContext): Tree = {
      NoTree
      val ident     = TreeFactories.mkIdent(Name(ctx.Identifier.getText),
                          pos(ctx.Identifier))
      if(ctx.selector == null) ident
      else {
        ctx.selector.asScala.toList.foldLeft(ident: Expr) { (z, y) =>
          if(y.Identifier != null) {
            val id = TreeFactories.mkIdent(Name(y.Identifier.getText),
                                           pos(y.Identifier))
            TreeFactories.mkSelect(z, id, pos(y))
          } else { // y.expression != null
            val expr   = visit(y.expression).asInstanceOf[Expr]
            TreeFactories.mkArrayAccess(z, expr)
          }
        }
      }
    }

    override def visitSimpleExpression2(
      ctx: Oberon0Parser.SimpleExpression2Context): Tree = {
      // INFO: No need to implement this
      visitChildren(ctx)
    }

    override def visitSimpleExpression(
      ctx: Oberon0Parser.SimpleExpressionContext): Tree = {
      val lhs = visit(ctx.term).asInstanceOf[Expr]
      val rhs = {
        if(ctx.simpleExpression2 == null) None
        else {
          val rhss = ctx.simpleExpression2.asScala.toList.reverse
          val z: Option[Expr => BinaryApi] = None
          rhss.foldLeft(z)((z, y) => {
            val op = y.op.getText match {
              case "+"               => Add
              case "-"               => Sub
              case "OR"              => Or
            }
            val e1 = visit(y.term).asInstanceOf[Expr]
            z match {
              case Some(f)       =>
                val rhs = f(e1)
                Some {
                  (lhs: Expr) =>
                    TreeFactories.mkBinary(lhs, op, rhs, pos(y.term))
                }
              case None          =>
                Some {
                  (lhs: Expr) =>
                    TreeFactories.mkBinary(lhs, op, e1, pos(y.term))
                }
            }
          })
        }
      }
      val expr = rhs.map(f => f(lhs)).getOrElse(lhs)
      ctx.sign match {
        case null                               =>
          expr
        case s if s.getText == "+"              =>
          TreeFactories.mkUnary(false, Pos, expr, pos(ctx))
        case s if s.getText == "-"              =>
          TreeFactories.mkUnary(false, Neg, expr, pos(ctx))
        case _                =>
          expr
      }
    }

    override def visitExpression(
      ctx: Oberon0Parser.ExpressionContext): Tree = {
      val lhs = visit(ctx.simpleExpression(0)).asInstanceOf[Expr]
      if(ctx.simpleExpression(1) == null) {
        lhs
      } else {
        val op = ctx.op.getText match {
          case "="    => Eq
          case "#"    => Neq
          case "<"    => Lt
          case "<="   => Le
          case ">"    => Gt
          case ">="   => Ge
        }
        val rhs = visit(ctx.simpleExpression(1)).asInstanceOf[Expr]
        TreeFactories.mkBinary(lhs, op, rhs, pos(ctx))
      }
    }

    override def visitAssignment(
      ctx: Oberon0Parser.AssignmentContext): Tree = {
      val lhs = visit(ctx.select).asInstanceOf[Expr]
      val rhs = visit(ctx.expression).asInstanceOf[Expr]
      TreeFactories.mkAssign(lhs, rhs, pos(ctx))
    }

    override def visitActualParameters(
      ctx: Oberon0Parser.ActualParametersContext): Tree = {
      // INFO: No need to implement this
      visitChildren(ctx)
    }


    override def visitProcedureCall(
      ctx: Oberon0Parser.ProcedureCallContext): Tree = {
      val fun = visit(ctx.select).asInstanceOf[Expr]
      if(ctx.actualParameters == null) fun
      else {
        val params =
          ctx.actualParameters.expression.asScala.toList.map { ctx =>
          visit(ctx).asInstanceOf[Expr]
        }
        TreeFactories.mkApply(fun, params, pos(ctx))
      }
    }

    override def visitElsep(
      ctx: Oberon0Parser.ElsepContext): Tree = {
      // INFO: No need to implement this
      visitChildren(ctx)
    }

    override def visitElseIf(
      ctx: Oberon0Parser.ElseIfContext): Tree = {
      // INFO: No need to implement this
      visitChildren(ctx)
    }

    override def visitIfStatement(
      ctx: Oberon0Parser.IfStatementContext): Tree = {
      val cond      = visit(ctx.expression).asInstanceOf[Expr]
      val thenp     = visit(ctx.statementSequence).asInstanceOf[BlockApi]
      val elseIfs   =
        if(ctx.elseIf == null) Nil
        else {
          ctx.elseIf.asScala.toList.map {ctx =>
            val cond  = visit(ctx.expression).asInstanceOf[Expr]
            val thenp = visit(ctx.statementSequence).asInstanceOf[BlockApi]
            (cond, thenp)
          }
        }
      val elsep: Expr =
        if(ctx.elsep.statementSequence == null) NoTree
        else {
          visit(ctx.elsep.statementSequence).asInstanceOf[BlockApi]
        }

      ((cond,thenp)::elseIfs).foldRight(elsep) {(y, z) =>
        TreeFactories.mkIf(y._1, y._2, z, y._1.pos)
      }
    }

    override def visitWhileStatement(
      ctx: Oberon0Parser.WhileStatementContext): Tree = {
      val cond = visit(ctx.expression).asInstanceOf[Expr]
      val body = visit(ctx.statementSequence).asInstanceOf[BlockApi]
      TreeFactories.mkWhile(false, cond, body, pos(ctx))
    }

    override def visitStatement(ctx: Oberon0Parser.StatementContext): Tree = {
      if(ctx.assignment != null) visit(ctx.assignment)
      else if(ctx.procedureCall != null) visit(ctx.procedureCall)
      else if(ctx.ifStatement != null) visit(ctx.ifStatement)
      else if(ctx.whileStatement != null) visit(ctx.whileStatement)
      else NoTree
    }

    override def visitStatementSequence(
      ctx: Oberon0Parser.StatementSequenceContext): Tree = {
      val stmts = ctx.statement.asScala.toList.map(visit(_))
      TreeFactories.mkBlock(stmts, pos(ctx))
    }

    override def visitIdentList(ctx: Oberon0Parser.IdentListContext): Tree = {
      // INFO: No need to implement this
      visitChildren(ctx)
    }

    override def visitArrayType(ctx: Oberon0Parser.ArrayTypeContext): Tree = {
      val expr = visit(ctx.expression).asInstanceOf[Expr]
      val tuse = visit(ctx.`type`).asInstanceOf[UseTree]
      TreeFactories.mkArrayTypeUse(tuse, expr, pos(ctx))
    }

    override def visitFieldList(ctx: Oberon0Parser.FieldListContext): Tree = {
      // INFO: No need to implement this
      visitChildren(ctx)
    }

    override def visitRecordType(
      ctx: Oberon0Parser.RecordTypeContext): Tree = {
      val fields = ctx.fieldList.asScala.toList.flatMap { ctx =>
        if(ctx.identList == null) {
          Nil
        } else {
          val tpt    = visit(ctx.`type`).asInstanceOf[UseTree]
          val p      = pos(ctx)
          ctx.identList.Identifier().asScala.toList.map { ctx =>
            val name = Name(ctx.getText)
            TreeFactories.mkValDef(noflags, tpt, name, NoTree, pos(ctx))
          }
        }
      }
      val template = TreeFactories.mkTemplate(fields, pos(ctx))
      TreeFactories.mkRecordDef(template, pos(ctx))
    }

    override def visitType(ctx: Oberon0Parser.TypeContext): Tree = {
      if(ctx.arrayType != null) visit(ctx.arrayType)
      else if(ctx.recordType != null) visit(ctx.recordType)
      else /* if(ctx.Identifier != null) */
       TreeFactories.mkTypeUse(Name(ctx.Identifier.getText), pos(ctx))
    }

    override def visitFpSection(ctx: Oberon0Parser.FpSectionContext): Tree = {
      // INFO: No need to implement this
      visitChildren(ctx)
    }

    override def visitFormalParameters(
      ctx: Oberon0Parser.FormalParametersContext): Tree = {
      // INFO: No need to implement this
      visitChildren(ctx)
    }

    override def visitProcedureHeading(
      ctx: Oberon0Parser.ProcedureHeadingContext): Tree = {
      // INFO: No need to implement this
      visitChildren(ctx)
    }

    override def visitProcedureBody(
      ctx: Oberon0Parser.ProcedureBodyContext): Tree = {
      // INFO: No need to implement this
      visitChildren(ctx)
    }

    override def visitProcedureDeclaration(
      ctx: Oberon0Parser.ProcedureDeclarationContext): Tree = {
      // INFO: No need to implement this
      visitChildren(ctx)
    }

    override def visitDeclarations(
      ctx: Oberon0Parser.DeclarationsContext): Tree = {
      // INFO: No need to implement this
      visitChildren(ctx)
    }

    override def visitConstDeclaration(
      ctx: Oberon0Parser.ConstDeclarationContext): Tree = {
      // INFO: No need to implement this
      visitChildren(ctx)
    }

    override def visitTypeDeclaration(
      ctx: Oberon0Parser.TypeDeclarationContext): Tree = {
      // INFO: No need to implement this
      visitChildren(ctx)
    }

    override def visitVarDeclaration(
      ctx: Oberon0Parser.VarDeclarationContext): Tree = {
      // INFO: No need to implement this
      visitChildren(ctx)
    }

    def parseDeclerations(
      ctx: Oberon0Parser.DeclarationsContext): List[DefTree] = {
      val consts =
        if(ctx.constDeclaration == null) Nil
        else {
          val names = ctx.constDeclaration.Identifier.asScala
          val exprs = ctx.constDeclaration.expression.asScala
          names.zip(exprs).toList.map { c =>
            val mods = FIELD | FINAL
            val name = Name(c._1.getText)
            val tpt  = TreeFactories.mkTypeUse(UNRESOLVED, pos(c._1))
            val rhs  = visit(c._2).asInstanceOf[Expr]
            TreeFactories.mkValDef(mods, tpt, name, rhs, pos(c._1))
          }
        }

      val types =
        if(ctx.typeDeclaration == null) Nil
        else {
          val names = ctx.typeDeclaration.Identifier.asScala
          val exprs = ctx.typeDeclaration.`type`.asScala
          names.zip(exprs).toList.map { c =>
            val name = Name(c._1.getText)
            val tpt  = visit(c._2)
            TreeFactories.mkTypeDef(name, tpt, pos(c._1))
          }
        }

      val (types2, vars) = {
        val res: List[(Option[TypeDefApi], List[ValDefApi])] =
          if(ctx.varDeclaration == null) Nil
          else {
            val names       = ctx.varDeclaration.identList.asScala
            val exprs       = ctx.varDeclaration.`type`.asScala
            names.zip(exprs).toList.map { c =>
              val mods           = Flags(FIELD)
              val names          =
                c._1.Identifier.asScala.toList.map(i => Name(i.getText))
              val (tdef, tpt)    = visit(c._2) match {
                case use: UseTree      =>
                  (None, use)
                case tpt               =>
                  val name = nextTypeName()
                  val tdef =
                    TreeFactories.mkTypeDef(name, tpt, pos(c._1))
                  val tuse =
                    TreeFactories.mkTypeUse(name, pos(c._1))
                  (Some(tdef), tuse)
              }
              val vdefs = names.map { name =>
                TreeFactories.mkValDef(mods, tpt, name, NoTree, pos(c._1))
              }
              (tdef, vdefs)
            }
          }
        val (tdefss, varss) = res.unzip
        (tdefss.flatten, varss.flatten)
      }

      val allTypes = types ++ types2

      val moreDefs =
        if(ctx.procedureDeclaration == null) Nil
        else ctx.procedureDeclaration.asScala
            .toList.map { ctx =>
            if(ctx.procedureHeading.name.Identifier.getText !=
                ctx.procedureBody.name.Identifier.getText) {
              error(NAME_MISMATCH, ctx.procedureHeading.name.Identifier.getText,
                               ctx.procedureBody.name.Identifier.getText,
                               pos(ctx.procedureBody.name.Identifier))
          }
          val name            = Name(ctx.procedureHeading.name.Identifier.getText)
          val (types, params) = {
            if(ctx.procedureHeading.formalParameters == null) (Nil, Nil)
            else {
              val res = ctx.procedureHeading.formalParameters
                .fpSection.asScala.toList.map { ctx =>
                val names       =
                  ctx.identList.Identifier.asScala.toList
                val (tdef, tpt) = visit(ctx.`type`) match {
                  case use: UseTree      =>
                    (None, use)
                  case tpt               =>
                    val name = nextTypeName()
                    val tdef =
                      TreeFactories.mkTypeDef(name, tpt, pos(ctx.`type`))
                    val tuse =
                      TreeFactories.mkTypeUse(name, pos(ctx.`type`))
                    (Some(tdef), tuse)
                }
                val params = names.toList.map { name =>
                  val mods           = Flags(PARAM)
                  names.map { ctx =>
                    TreeFactories.mkValDef(
                      mods, tpt, Name(ctx.getText), NoTree, pos(ctx))
                  }
                }
                (tdef, params)
              }
              val (tdefs, paramss) = res.unzip
              (tdefs.flatten, paramss.flatten.flatten)
            }
          }
          val decls = parseDeclerations(
            ctx.procedureBody.declarations)
          val mctx = ctx.procedureBody.statementSequence
          val body = if(mctx == null)
                       TreeFactories.mkBlock(decls, pos(mctx))
                     else {
                       val b = visit(mctx).asInstanceOf[BlockApi]
                       TreeCopiers.copyBlock(b)(stmts = decls ++ b.stmts)
                     }
          val mdef = {
            val ret = TreeFactories.mkTypeUse(VOID_TYPE_NAME, pos(ctx))
            TreeFactories.mkMethodDef(ret, name, params, body, pos(ctx))
          }
          types ++ List(mdef)
        }


      consts ++ allTypes ++ vars ++ moreDefs.flatten
    }

    override def visitModule(ctx: Oberon0Parser.ModuleContext): Tree = {
      if(ctx.name(0).Identifier.getText != ctx.name(1).Identifier.getText) {
        error(NAME_MISMATCH, ctx.name(0).Identifier.getText,
                             ctx.name(1).Identifier.getText,
                             pos(ctx.name(1).Identifier))
      }
      val name     = Name(ctx.name(0).Identifier.getText)
      val body     = if(ctx.statementSequence == null)
                         None
                       else
                         Some(visit(
                           ctx.statementSequence).asInstanceOf[BlockApi])
      val decls = parseDeclerations(ctx.declarations)
      TreeFactories.mkModuleDef(name, decls,
          body, pos(ctx))
      }
  }
}


object Parser extends Parser
