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

package ch.usi.inf.l3.sana.dcct.parsers

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.ooj
import sana.dcct
import tiny.source.{SourceFile, Position}
import tiny.symbols._
import tiny.ast.{TreeCopiers => _, _}
import tiny.names.Name
import tiny.modifiers.Flags
import tiny.parsers
import tiny.debug.logger
import calcj.ast.{TreeCopiers => _, _}
import calcj.ast.operators._
import tiny.ast.NoTree
import primj.ast._
import dcct.antlr._
import primj.modifiers._
import primj.modifiers.Ops._
import ooj.names.StdNames._
import ooj.ast._
import dcct.ast.TreeFactories._
import dcct.ast._
import ooj.ast.Implicits._ // TODO could be a source of problems
import dcct.ast.Implicits._

import org.antlr.v4.runtime.misc.NotNull
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.AbstractParseTreeVisitor
import org.antlr.v4.runtime.tree.TerminalNode;



import scala.collection.JavaConverters._

// TODO not sure in general whether to use ooj or primj versions of 
// tree factories


class Parser extends tiny.parsers.Parser {


  def parse(source: SourceFile): Tree = {
    val tree = new DcctVisitor(source.name,
      source.lines).visit(source.content)
    logger.info(s"[PARSE TREE]\n $tree")
    // Program(tree, None, source.name)
    tree match {
      case program: primj.ast.ProgramApi =>
        primj.ast.TreeCopiers.copyProgram(program)(sourceName = source.name)
      case _                => tree
    }
  }

  class DcctVisitor(val source: String,
    lines: Array[String]) extends DcctBaseVisitor[Tree] {
    def pos(ctx: ParserRuleContext): Option[Position] = {
      val token = ctx.getStart
      Some(Position(source, lines,
        token.getLine, token.getCharPositionInLine + 1))
    }

     
    /**
     * Extract defs from the schema and return the program tree containing them.
     */
    override def visitProgram(@NotNull ctx: DcctParser.ProgramContext): Tree = {
      logger.info("Parsing program started...")
      // Extract entity and array defs from schema
      val schema =  ctx.schema()
      val actionDefs = ctx.actionDeclaration

      val actions = if(actionDefs != null) {
        actionDefs.asScala.toList.map {
          action => visit(action).asInstanceOf[DefTree]
        }
      } else Nil

      val cloudTypeDefs = if (schema != null) {
        ctx.schema().cloudDataDecl().asScala.toList.map {
          child => 
            val cloudDef =  visit(child)
            if (cloudDef.isInstanceOf[ClassDefApi])
             cloudDef.asInstanceOf[ClassDefApi] 
           else 
             cloudDef.asInstanceOf[ArrayDefApi] 
        }
        
      } else Nil
      
      primj.ast.TreeFactories.mkProgram(cloudTypeDefs ++ actions, source)

    }
/************************      Schema Related         ************************/

// TODO I believe I can delete everything that does only visit Children
    override def visitSchema(@NotNull ctx: DcctParser.SchemaContext): Tree = {
      visitChildren(ctx)
    }

    override def visitIndexType(@NotNull ctx: DcctParser.IndexTypeContext): UseTree = { 
      primj.ast.TreeFactories.mkTypeUse(Name(ctx.getText), pos(ctx))
    }
    
    override def visitCloudType(@NotNull ctx: DcctParser.CloudTypeContext): UseTree = { 
      val cloudType = primj.ast.TreeFactories.mkTypeUse(Name(ctx.cloudPrimType.getText), pos(ctx))

      if( ctx.annotationType != null ) 
        cloudType.consistencyAnnotation = Name(ctx.annotationType.getText)
      
      if(ctx.Identifier != null )
        cloudType.consistencyRegion = Name(ctx.Identifier.getText)

      cloudType
    }

    override def visitCloudDataDecl(@NotNull ctx: DcctParser.CloudDataDeclContext): Tree = {
      // TODO is this the right way to visit all kinds of cloud types?
      if(ctx.entityDecl != null ) { println("Entity Found "); visit(ctx.entityDecl()) }
      else if(ctx.arrayDecl != null) visit(ctx.arrayDecl()) 
      else  visitChildren(ctx); 
    }


    override def visitEntityDecl(@NotNull ctx: DcctParser.EntityDeclContext): Tree = {
      // TODO do not allow a table without any fields to be generated! by checking that
      // we have either at least one property or one element
       
      val elements: List[ValDefApi] = getElementsList(ctx.elements())      
      val properties: List[ValDefApi] = if (ctx.properties() != null) {
        ctx.properties().property().asScala.toList.map {
          property => visit(property).asInstanceOf[ValDefApi]
        }
      } else Nil
      
      
      mkClassDef(noflags, getIdentName(ctx.Identifier), 
        Nil, mkTemplate(elements ++ properties, pos(ctx.elements())), pos(ctx))
    }
    
    override def visitArrayDecl(@NotNull ctx: DcctParser.ArrayDeclContext): Tree = {
      val indices: List[ValDefApi] = getElementsList(ctx.elements())     
      val properties: List[ValDefApi] = if (ctx.properties() != null) {
        ctx.properties().property().asScala.toList.map {
          property => visit(property).asInstanceOf[ValDefApi]
        }
      } else Nil

      // TODO create the proper arraydef tree here.
     mkArrayDef(getIdentName(ctx.Identifier), indices, 
      properties)  
    }
    
    override def visitElements(@NotNull ctx: DcctParser.ElementsContext): Tree = {
      visitChildren(ctx)
    }

    
    override def visitElement(@NotNull ctx: DcctParser.ElementContext): ValDefApi = {
      val tpe = visitIndexType(ctx.indexType())
      // Param here indicates that the entity we are currently processing is weak,
      // and when the entity it is dependent on is removed (any ValDef with PARAM) 
      // flag, we have to delete this entity as well. 
      
      // TODO we have a problem here, maybe I want to pass parameters as well as a 
      // dependency! the language under question does not allow that, however, I will
      // ignore that for now, and later will distinguish between these two by
      // defining a new flag. In this case PARAM will be a parameter, 
      // DEPENDENCY will be the dependency!
      
      ooj.ast.TreeFactories.mkValDef(
        Flags(PARAM), tpe, getIdentName(ctx.Identifier), NoTree, pos(ctx))
    }
    
    override def visitProperties(@NotNull ctx: DcctParser.PropertiesContext): Tree = {
      visitChildren(ctx)
    }

    
    override def visitProperty(@NotNull ctx: DcctParser.PropertyContext): Tree = {
      val tpe = visitCloudType(ctx.cloudType())
      // TODO maybe better flag the property types?
      ooj.ast.TreeFactories.mkValDef(noflags, tpe, getIdentName(ctx.Identifier), NoTree, pos(ctx))
    }
    
/************************      Actions and Statements        ************************/

   override def visitActionDeclaration(@NotNull 
     ctx: DcctParser.ActionDeclarationContext): Tree = {   

      val tpe    = visit(ctx.indexType)
      val name   = ctx.Identifier.getText
      val params = getElementsList(ctx.elements())
      val body   = visit(ctx.block)
      (tpe, body) match {
        case (tu: TypeUseApi, b: BlockApi) =>
          mkActionDef(tu, Name(name), params, b,
            pos(ctx))
        case _                       =>
          // TODO: report an error
          throw new Exception(s"Bad action definition ${pos(ctx)}")
      }
    }

  override def visitBlock(@NotNull ctx: DcctParser.BlockContext): Tree = { 
      val expressions =  ctx.expressions.expression.asScala.toList.map {
          expression => visit(expression)
        }
     mkBlock(expressions, pos(ctx))   
  }
  

/************************      Expressions        ************************/

/*
 * expression.asScala.toList.map(
    e => visit(e).asInstanceOf[Expr])

 */
/////////////// Binary Visitors 
  override def visitMul(@NotNull ctx: DcctParser.MulContext): Tree = {
      createBinary(ctx.expression, ctx.op.getText, ctx)
  }

  override def visitAdd(@NotNull ctx: DcctParser.AddContext): Tree = {
      createBinary(ctx.expression, ctx.op.getText, ctx)
  }


  override def visitRel(@NotNull ctx: DcctParser.RelContext): Tree = {
      createBinary(ctx.expression, ctx.op.getText, ctx)
  }

  override def visitEqu(@NotNull ctx: DcctParser.EquContext): Tree = {
      createBinary(ctx.expression, ctx.op.getText, ctx)
  }

  override def visitAnd(@NotNull ctx: DcctParser.AndContext): Tree =  {
      createBinary(ctx.expression, "&&", ctx)
  }

  override def visitOr(@NotNull ctx: DcctParser.OrContext): Tree =  {
      createBinary(ctx.expression, "||", ctx)
  }

/////////////// Unary Visitors 
  override def visitUnaryNum(@NotNull ctx: DcctParser.UnaryNumContext): Tree = {
      createUnaryOrPostfix(false, ctx.expression, ctx.op.getText, ctx)
  }

  override def visitUnaryBool(@NotNull ctx: DcctParser.UnaryBoolContext): Tree = {
      createUnaryOrPostfix(false, ctx.expression, ctx.getText, ctx)
  }

///////////// Desugared to an apply. 
  override def visitAllEntitesOrArrayElem(
   @NotNull ctx: DcctParser.AllEntitesOrArrayElemContext): Tree = {
      val ident =  mkIdent(Name("ALL"), pos(ctx))
      val arg = mkIdent((Name(ctx.Identifier.getText)))
      mkApply(ident, List(arg), pos(ctx))
  }

  override def visitDeleteEntity(@NotNull ctx: DcctParser.DeleteEntityContext): Tree = {
    val ident =  mkIdent(Name("DELETE"), pos(ctx))
    val arg = visit(ctx.expression).asInstanceOf[Expr]
    mkApply(ident, List(arg), pos(ctx))
  }


  override def visitArraySelector(@NotNull ctx: DcctParser.ArraySelectorContext): Tree = {
    // TODO this could equivelant to an SQL/CQL select or update, depending
    // whether it appears in an assignment statement.
    // I believe I need to check this within an assignment statement or
    // something
    val ident =  mkIdent(Name("SELECT"), pos(ctx))
    val args = ctx.expressionArgs match {
        case null                       => Nil
        case list                       =>
          list.expression.asScala.toList.map { (x) =>
            visit(x).asInstanceOf[Expr]
          }
      }
    mkApply(ident, args, pos(ctx))
 
  }

/////////////// Other expressions
  override def visitIdentifier(@NotNull ctx: DcctParser.IdentifierContext ) : Tree = {
   mkIdent(getIdentName(ctx.Identifier))
  }

  override def visitParExpr(@NotNull ctx: DcctParser.ParExprContext ) : Tree = {
    visit(ctx.expression).asInstanceOf[Expr]
  }
// TODO maybe just create a method getIdent! 
  override def visitEntityArraySelect(@NotNull ctx: DcctParser.EntityArraySelectContext ) : Tree = {
   mkSelect(mkIdent(getIdentName(ctx.Identifier(0))), mkIdent(getIdentName(ctx.Identifier(1))))
  }


  override def visitNewEntity(@NotNull ctx: DcctParser.NewEntityContext ) : Tree = {
    val qual    = mkIdent(getIdentName(ctx.Identifier))
    val ps      = pos(ctx)
    val init    = mkIdent(CONSTRUCTOR_NAME, ps)
    init.isConstructorIdent = true
    val fun     = mkSelect(qual, init, ps)
    val args = ctx.expressionArgs match {
        case null                       => Nil
        // TODO a helper for this too is better
        case list                       =>
          list.expression.asScala.toList.map { (x) =>
            visit(x).asInstanceOf[Expr]
          }
      }
      val app = mkApply(fun, args, ps)
      mkNew(app, ps)
 
  }

  override def visitBranching(@NotNull ctx: DcctParser.BranchingContext ) : Tree = {
    val cond  = visit(ctx.expression)
    val thenp = ctx.block
    val elsep = ctx.block match {
      case null => NoTree
      case block => block
    }
  
    //TODO not sure how this would work
    (cond, thenp, elsep) match {
      case (c: Expr, t: Expr, e: Expr) =>
        mkIf(c, t, e, pos(ctx))
      case _                           =>
        // TODO: report an error
        throw new Exception("Bad tree shape")
    }
  }
  
  override def visitActionCall(@NotNull ctx: DcctParser.ActionCallContext ) : Tree = {
      val applyLHS     = visit(ctx.expression).asInstanceOf[Expr]
      val args   = ctx.expressionArgs match {
        case null           => Nil
        case args           =>
          args.expression.asScala.toList.map {
            case e => visit(e).asInstanceOf[Expr]
          }
        }
      mkApply(applyLHS, args, pos(ctx))
  }

  override def visitIntLit(@NotNull ctx: DcctParser.IntLitContext): Tree = {
    mkLiteral(IntConstant(ctx.getText.toInt), pos(ctx))
  }

  override def visitStringLit(@NotNull ctx: DcctParser.StringLitContext): Tree = {
    mkLiteral(StringConstant(ctx.getText), pos(ctx))
  }

  
  override def visitValDecl(@NotNull ctx: DcctParser.ValDeclContext): Tree = {
    val mods = noflags      
    val tpe = visitIndexType(ctx.indexType)
    val name = getIdentName(ctx.Identifier)
    val rhs =  visit(ctx.expression).asInstanceOf[Expr]
    mkValDef(mods, tpe, name, rhs, pos(ctx))
  }

  override def visitAssign(@NotNull ctx: DcctParser.AssignContext): Tree = {
    // either an ident or selector
    val lhs = visit(ctx.expression(0)).asInstanceOf[Expr]
    // TODO maybe put a catch for cast exceptions?
    val rhs = visit(ctx.expression(1)).asInstanceOf[Expr]
    mkAssign(lhs, rhs, pos(ctx))
    
      
  }

  override def visitForeach(@NotNull ctx: DcctParser.ForeachContext): Tree = {
    val varIdent = getIdentName(ctx.Identifier(0))
    val whereExpr = visit(ctx.expression).asInstanceOf[Expr]
    val entitySelected = getIdentName(ctx.Identifier(1))
    val entityVarType = primj.ast.TreeFactories.mkTypeUse(entitySelected, pos(ctx))
    val entityVar =  mkValDef(noflags, entityVarType, varIdent, NoTree, pos(ctx))
    val block = visit(ctx.block).asInstanceOf[BlockApi]
      
    mkForEach(entityVar, whereExpr: Expr, block)
 
  }



/********************************       Helpers       *****************************/
  def createUnaryOrPostfix[T <: ParserRuleContext](isPostfix: Boolean,
      exp: T, trm: String, ctx: ParserRuleContext): Expr = {

      val e1 = visit(exp)
      val op = trm match {
        case "-"     => Neg
        case "+"     => Pos
        case "++"    => Inc
        case "--"    => Dec
        case "~"     => BCompl
        case "!"     => Not
      }
      (e1, op) match {
        case (e: Expr, op: POp) if isPostfix =>
          mkUnary(true, op, e, pos(ctx))
        case (e: Expr, op: UOp) =>
          mkUnary(false, op, e, pos(ctx))
        case _                  =>
          // TODO: report an error
          throw new Exception(
            "Expression is expected, but got " + e1 + " " + op)
      }
    }
 
  def createBinary[T <: ParserRuleContext](es: java.util.List[T],
      trm: String, ctx: ParserRuleContext): Expr = {
      val e1 = visit(es.get(0))
      val op = trm match {
        case "*"     => Mul
        case "/"     => Div
        case "%"     => Mod
        case "+"     => Add
        case "-"     => Sub
        case "<"     => Lt
        case ">"     => Gt
        case "<="    => Le
        case ">="    => Ge
        case "=="    => Eq
        case "!="    => Neq
        case "&&"    => And
        case "||"    => Or
      }
      val e2 = visit(es.get(1))
      (e1, e2) match {
        case (x: Expr, y: Expr) =>
          primj.ast.TreeFactories.mkBinary(x, op, y, pos(ctx))
        case _                  =>
          // TODO: report an error
          throw new Exception("Expression is expected but got: EXP1 " + e1 + ", EXP2 " + e2)
      }
    }

  def getElementsList(ctx: DcctParser.ElementsContext): List[ValDefApi] = {
    if (ctx != null) {
        ctx.element().asScala.toList.map {
          element => visit(element).asInstanceOf[ValDefApi]
        }
      } else Nil  
    }

  def getIdentName(ident: TerminalNode ): Name = {
    Name(ident.getText)
  }
  }
}

object Parser extends Parser
