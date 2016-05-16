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
import primj.ast.TreeFactories._
import dcct.antlr._
import primj.modifiers._
import primj.modifiers.Ops._
import ooj.ast._
import ooj.ast.TreeFactories._


import org.antlr.v4.runtime.misc.NotNull
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.AbstractParseTreeVisitor


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
          primj.ast.TreeFactories.mkUnary(true, op, e, pos(ctx))
        case (e: Expr, op: UOp) =>
          primj.ast.TreeFactories.mkUnary(false, op, e, pos(ctx))
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
        case "<<"    => SHL
        case ">>"    => SHR
        case ">>>"   => USHR
        case "<"     => Lt
        case ">"     => Gt
        case "<="    => Le
        case ">="    => Ge
        case "=="    => Eq
        case "!="    => Neq
        case "&"     => BAnd
        case "^"     => BXor
        case "|"     => BOr
        case "&&"    => And
        case "||"    => Or
      }
      val e2 = visit(es.get(1))
      (e1, e2) match {
        case (x: Expr, y: Expr) =>
          primj.ast.TreeFactories.mkBinary(x, op, y, pos(ctx))
        case _                  =>
          // TODO: report an error
          throw new Exception("Expression is expected but got: " + e1 + " " + e2)
      }
    }
    
    /**
     * Extract defs from the scehema and return the program tree containing them.
     */
    override def visitProgram(@NotNull ctx: DcctParser.ProgramContext): Tree = {
      logger.info("Parsing program started...")
      // Extract entity and array defs from schema
      val schema =  ctx.schema()

      val cloudTypeDefs = if (schema != null) {
        val decls = ctx.schema().cloudDataDecl().asScala.toList.map {
          child => visit(child).asInstanceOf[ClassDefApi]
        }
        Some(decls)
      } else None
      
      primj.ast.TreeFactories.mkProgram(cloudTypeDefs.getOrElse(Nil), source)

    }

    override def visitSchema(@NotNull ctx: DcctParser.SchemaContext): Tree = {
      visitChildren(ctx)
    }

    override def visitIndexType(@NotNull ctx: DcctParser.IndexTypeContext): Tree = { 
      visitChildren(ctx)
    }
    
    override def visitCloudType(@NotNull ctx: DcctParser.CloudTypeContext): Tree ={ 
      visitChildren(ctx); 
    }

    override def visitCloudDataDecl(@NotNull ctx: DcctParser.CloudDataDeclContext): Tree = {
      // TODO is this the right way to visit all kinds of cloud types?
      visit(ctx.entityDecl()) 
//      if(ctx.arrayDecl() != null) visit(ctx.arrayDecl()) 
    }


    override def visitEntityDecl(@NotNull ctx: DcctParser.EntityDeclContext): Tree = {
      // TODO do not allow a table without any fields to be generated! by checking that
      // we have either at least one property or one element
      val entityIdent = ctx.Identifier().getText

      val elements: List[ValDefApi] = if (ctx.elements() != null) {
        ctx.elements().element().asScala.toList.map {
          element => visit(element).asInstanceOf[ValDefApi]
        }
      } else Nil
      
      val properties: List[ValDefApi] = if (ctx.properties() != null) {
        ctx.properties().property().asScala.toList.map {
          property => visit(property).asInstanceOf[ValDefApi]
        }
      } else Nil
      
      
      mkClassDef(noflags, Name(entityIdent), Nil, mkTemplate(elements ++ properties, pos(ctx.elements())), pos(ctx))
    }
    
    override def visitArrayDecl(@NotNull ctx: DcctParser.ArrayDeclContext): Tree = {
      val arrayIdent = ctx.Identifier().getText
      val elements = ctx.elements().element().asScala.toList.map {
        element => visit(element).asInstanceOf[ValDefApi] // correct type?
      }
      // TODO create the proper arraydef tree here.
      mkClassDef(noflags, Name(arrayIdent), Nil, mkTemplate(elements, pos(ctx.elements())), pos(ctx))
    }
    
    override def visitElements(@NotNull ctx: DcctParser.ElementsContext): Tree = {
      visitChildren(ctx)
    }

    
    override def visitElement(@NotNull ctx: DcctParser.ElementContext): Tree = {
      val elemIdent = ctx.Identifier()
      val elemType = ctx.indexType()
      
      val tpe = primj.ast.TreeFactories.mkTypeUse(Name(elemType.getText), pos(ctx))
      val name = Name(ctx.Identifier.getText)
      // Param here indicates that the entity we are currently processing is weak,
      // and when the entity it is dependent on is removed (any ValDef with PARAM) 
      // flag, we have to delete this entity as well. 
      
      // TODO we have a problem here, maybe I want to pass parameters as well as a 
      // dependency! the language under question does not allow that, however, I will
      // ignore that for now, and later will distinguish between these two by
      // defining a new flag. In this case PARAM will be a parameter, 
      // DEPENDENCY will be the dependency!
      
      ooj.ast.TreeFactories.mkValDef(Flags(PARAM), tpe, name, NoTree, pos(ctx))
    }
    
    override def visitProperties(@NotNull ctx: DcctParser.PropertiesContext): Tree = {
      visitChildren(ctx)
    }

    
    override def visitProperty(@NotNull ctx: DcctParser.PropertyContext): Tree = {
      val propIdent = ctx.Identifier()
      val propType = ctx.cloudType()
      // TODO add the cloud types to my standard defs 
      val tpe = primj.ast.TreeFactories.mkTypeUse(Name(propType.getText), pos(ctx))
      val name = Name(ctx.Identifier.getText)
      // TODO maybe better flag the property types?
      ooj.ast.TreeFactories.mkValDef(noflags, tpe, name, NoTree, pos(ctx))
    }


//    override def visitAssign(@NotNull ctx: DcctParser.AssignContext): Tree = {
//      val name   = ctx.Identifier.getText
//      val id     = mkIdent(Name(name), pos(ctx))
//      val e2     = visit(ctx.expression).asInstanceOf[Expr]
//      val op: Option[BOp] = ctx.op.getText match {
//        case "+="   => Some(Add)
//        case "-="   => Some(Sub)
//        case "*="   => Some(Mul)
//        case "/="   => Some(Div)
//        case "%="   => Some(Mod)
//        case "&="   => Some(BAnd)
//        case "|="   => Some(BOr)
//        case "^="   => Some(BXor)
//        case "<<="  => Some(SHL)
//        case ">>="  => Some(SHR)
//        case ">>>=" => Some(USHR)
//        case "="    => None
//      }
//      op match {
//        case None     =>
//          mkAssign(id, e2, pos(ctx))
//        case Some(op) =>
//          val rhs = mkBinary(id, op, e2, pos(ctx))
//          mkAssign(id, rhs, pos(ctx))
//      }
//    }
//


//    def createVarDecls(@NotNull ctx:
//      DcctParser.VariableDeclarationContext,
//      mods: Flags): List[ValDefApi] = {
//      val mods1   = if(ctx.mods != null)
//                      mods | FINAL
//                    else mods
//      val tpe    = visit(ctx.`type`)
//      val names  = ctx.Identifier.asScala.toList.map(_.getText)
//      val exprs  = ctx.varRHS.asScala.toList.map {
//        case null => NoTree
//        case e    => visit(e).asInstanceOf[Expr]
//      }
//      tpe match {
//        case tu: TypeUse =>
//          names.zip(exprs).map {
//            case (name, expr) =>
//              mkValDef(mods1, tu, Name(name), expr, pos(ctx))
//          }
//        case _           =>
//          // TODO: report an error
//          throw new Exception("TypeUse is expected")
//      }
//    }
//
//    def createVarDefs(@NotNull ctx:
//      DcctParser.VariableDefinitionContext,
//      mods: Flags): List[ValDefApi] = {
//
//      val mods1   = if(ctx.mods != null)
//                      mods | FINAL
//                    else
//                      mods
//      val tpe    = visit(ctx.`type`)
//      val names  = ctx.Identifier.asScala.toList.map(_.getText)
//      val exprs  = ctx.expression.asScala.toList.map {
//        case es => visit(es).asInstanceOf[Expr]
//      }
//      tpe match {
//        case tu: TypeUseApi =>
//          names.zip(exprs).map {
//            case (name, expr) =>
//              mkValDef(mods1, tu, Name(name), expr, pos(ctx))
//          }
//        case _           =>
//          // TODO: report an error
//          throw new Exception("Expression is expected")
//      }
//    }
//
//    override def visitFormalParameter(@NotNull ctx:
//      DcctParser.FormalParameterContext): Tree = {
//      val mods    = if(ctx.mods != null)
//                      PARAM | FINAL
//                    else
//                      Flags(PARAM)
//      val tpe = mkTypeUse(Name(ctx.`type`.getText), pos(ctx))
//      val name = Name(ctx.Identifier.getText)
//      mkValDef(mods, tpe, name, NoTree, pos(ctx))
//    }
//
//
//		override def visitMethodDeclaration(@NotNull ctx:
//      DcctParser.MethodDeclarationContext): Tree = {
//      val tpe    = visit(ctx.`type`)
//      val name   = ctx.Identifier.getText
//      val params = ctx.formalParameters.formalParameterList match {
//        case null                                      => List()
//        case ps if ps.formalParameter != null          =>
//          ps.formalParameter.asScala.toList.map {
//            case e  => visit(e).asInstanceOf[ValDefApi]
//          }
//        case _                                         => List()
//      }
//      val body   = visit(ctx.methodBody)
//      (tpe, body) match {
//        case (tu: TypeUseApi, b: BlockApi) =>
//          mkMethodDef(tu, Name(name), params, b,
//            pos(ctx))
//        case _                       =>
//          // TODO: report an error
//          throw new Exception("Bad tree shape")
//      }
//    }
//
//    override def visitVarRHS(@NotNull ctx: DcctParser.VarRHSContext): Tree = {
//      if(ctx.expression == null) NoTree
//      else visit(ctx.expression)
//    }
//
//
//		override def visitVoidType(@NotNull ctx: DcctParser.VoidTypeContext): Tree = {
//      mkTypeUse(Name("void"), pos(ctx))
//    }
//
//		override def visitBlock(@NotNull ctx: DcctParser.BlockContext): Tree = {
//      val stmts   = ctx.statement match {
//        case null    => Nil
//        case stmts   => stmts.asScala.toList.flatMap { stmt =>
//          stmt match {
//            case vr: DcctParser.VarStmtContext =>
//              createVarDecls(vr.variableDeclaration,
//                Flags(LOCAL_VARIABLE))
//            case _                  =>
//              List(visit(stmt))
//          }
//        }
//      }
//      mkBlock(stmts, pos(ctx))
//    }
//
//		override def visitIf(@NotNull ctx: DcctParser.IfContext): Tree = {
//      val cond  = visit(ctx.parExpression)
//      val thenp = visit(ctx.statement.get(0))
//      val elsep = ctx.statement.size match {
//        case 2 => visit(ctx.statement.get(1))
//        case 1 => NoTree
//      }
//      (cond, thenp, elsep) match {
//        case (c: Expr, t: Expr, e: Expr) =>
//          mkIf(c, t, e, pos(ctx))
//        case _                           =>
//          // TODO: report an error
//          throw new Exception("Bad tree shape")
//      }
//    }
//		override def visitFor(@NotNull ctx: DcctParser.ForContext): Tree = {
//      val inits = ctx.forControl.forInit match {
//        case null  => Nil
//        case inits =>
//          if(inits.expressionList != null)
//            visit(inits.expressionList)
//                .asInstanceOf[java.util.List[Tree]].asScala.toList
//          else
//            createVarDefs(inits.variableDefinition,
//              Flags(LOCAL_VARIABLE))
//      }
//      val cond  = ctx.forControl.expression match {
//        case null => NoTree
//        case e    => visit(e)
//      }
//      val steps = ctx.forControl.forUpdate match {
//        case null  => Nil
//        case es    =>
//          es.expressionList.expression.asScala.toList map {
//            case e => visit(e).asInstanceOf[Expr]
//          }
//      }
//      val body  = visit(ctx.statement)
//      (cond, body) match {
//        case (c: Expr, b: Expr) =>
//          mkFor(inits, c, steps, b, pos(ctx))
//        case _                  =>
//          // TODO: report an error
//          throw new Exception("Bad tree shape")
//      }
//    }
//		override def visitWhile(@NotNull ctx: DcctParser.WhileContext): Tree = {
//      val cond = visit(ctx.parExpression)
//      val body = visit(ctx.statement)
//      (cond, body) match {
//        case (c: Expr, b: Expr) =>
//          mkWhile(false, c, b, pos(ctx))
//        case _                  =>
//          // TODO: report an error
//          throw new Exception("Bad tree shape")
//      }
//    }
//		override def visitDoWhile(@NotNull ctx: DcctParser.DoWhileContext): Tree = {
//      val cond = visit(ctx.parExpression)
//      val body = visit(ctx.statement)
//      (cond, body) match {
//        case (c: Expr, b: Expr) =>
//          mkWhile(true, c, b, pos(ctx))
//        case _                  =>
//          // TODO: report an error
//          throw new Exception("Bad tree shape")
//      }
//    }
//
//		override def visitReturn(@NotNull ctx: DcctParser.ReturnContext): Tree = {
//      ctx.expression match {
//        case null                =>
//          mkReturn(None, pos(ctx))
//        case expr                =>
//          val e = visit(expr)
//          e match {
//            case e: Expr         =>
//              mkReturn(Some(e), pos(ctx))
//          case _                 =>
//            // TODO: report an error
//            throw new Exception("Bad tree shape")
//          }
//      }
//    }
//
//    override def visitBlockStmt(@NotNull ctx:
//      DcctParser.BlockStmtContext): Tree = {
//      visit(ctx.block)
//    }
//
//    override def visitExprStmt(@NotNull ctx: DcctParser.ExprStmtContext): Tree = {
//      visit(ctx.expression)
//    }
//
//    override def visitVarStmt(@NotNull ctx: DcctParser.VarStmtContext): Tree = {
//      visit(ctx.variableDeclaration)
//    }
//
//    override def visitAssignStmt(@NotNull ctx:
//      DcctParser.AssignStmtContext): Tree = {
//      visit(ctx.assign)
//    }
//
//    override def visitEmpty(@NotNull ctx: DcctParser.EmptyContext): Tree = {
//      NoTree
//    }
//		override def visitTernary(@NotNull ctx: DcctParser.TernaryContext): Tree = {
//      val cond  = visit(ctx.parExpression)
//      val thenp = visit(ctx.expression.get(0))
//      val elsep = visit(ctx.expression.get(1))
//      (cond, thenp, elsep) match {
//        case (c: Expr, t: Expr, e: Expr) =>
//          mkTernary(c, t, e, pos(ctx))
//        case _                           =>
//          // TODO: report an error
//          throw new Exception("Bad tree shape")
//      }
//    }
//		override def visitApply(@NotNull ctx: DcctParser.ApplyContext): Tree = {
//      val name   = ctx.Identifier.getText
//      val id     = mkIdent(Name(name), pos(ctx))
//      val args   = ctx.arguments.expressionList match {
//        case null           => Nil
//        case args           =>
//          args.expression.asScala.toList.map {
//            case e => visit(e).asInstanceOf[Expr]
//          }
//        }
//      mkApply(id, args, pos(ctx))
//    }
//
//
//    override def visitUnaryNum(@NotNull ctx: DcctParser.UnaryNumContext): Tree = {
//      createUnaryOrPostfix(false, ctx.expression, ctx.op.getText, ctx)
//    }
//
//    override def visitUnaryElse(@NotNull ctx: DcctParser.UnaryElseContext): Tree = {
//      createUnaryOrPostfix(false, ctx.expression, ctx.op.getText, ctx)
//    }
//
//    override def visitPostfix(@NotNull ctx: DcctParser.PostfixContext): Tree = {
//      createUnaryOrPostfix(true, ctx.expression, ctx.op.getText, ctx)
//    }
//
//    override def visitId(@NotNull ctx: DcctParser.IdContext): Tree = {
//      mkIdent(Name(ctx.getText), pos(ctx))
//    }
//
//    override def visitPrimitiveType(
//      @NotNull ctx: DcctParser.PrimitiveTypeContext): Tree = {
//      mkTypeUse(Name(ctx.getText), pos(ctx))
//    }
//
//    override def visitCast(@NotNull ctx: DcctParser.CastContext): Tree = {
//      val e = visit(ctx.expression)
//      val tpt = visit(ctx.primitiveType)
//      (tpt, e) match {
//        case (tpt: TypeUseApi, e: Expr) =>
//          mkCast(tpt, e, pos(ctx))
//        case _               =>
//          // TODO: report an error
//          throw new Exception("(TypeUse) Expression is expected")
//      }
//    }
//
//
//    // Binary visitors
//
//    override def visitMul(@NotNull ctx: DcctParser.MulContext): Tree = {
//      createBinary(ctx.expression, ctx.op.getText, ctx)
//    }
//
//    override def visitAdd(@NotNull ctx: DcctParser.AddContext): Tree = {
//      createBinary(ctx.expression, ctx.op.getText, ctx)
//    }
//
//    override def visitShifts(@NotNull ctx: DcctParser.ShiftsContext): Tree = {
//      createBinary(ctx.expression, ctx.op.getText, ctx)
//    }
//
//    override def visitRel(@NotNull ctx: DcctParser.RelContext): Tree = {
//      createBinary(ctx.expression, ctx.op.getText, ctx)
//    }
//
//    override def visitEqu(@NotNull ctx: DcctParser.EquContext): Tree = {
//      createBinary(ctx.expression, ctx.op.getText, ctx)
//    }
//
//    override def visitBAnd(@NotNull ctx: DcctParser.BAndContext): Tree = {
//      createBinary(ctx.expression, "&", ctx)
//    }
//
//    override def visitBXor(@NotNull ctx: DcctParser.BXorContext): Tree = {
//      createBinary(ctx.expression, "^", ctx)
//    }
//
//    override def visitBOr(@NotNull ctx: DcctParser.BOrContext): Tree = {
//      createBinary(ctx.expression, "|", ctx)
//    }
//
//    override def visitAnd(@NotNull ctx: DcctParser.AndContext): Tree =  {
//      createBinary(ctx.expression, "&&", ctx)
//    }
//
//    override def visitOr(@NotNull ctx: DcctParser.OrContext): Tree =  {
//      createBinary(ctx.expression, "||", ctx)
//    }
//
//
//
//
//    // Literaleral visitors
//
//    override def visitIntLit(@NotNull ctx: DcctParser.IntLitContext): Tree = {
//      val txt = ctx.getText
//      (txt.endsWith("l") || txt.endsWith("L")) match {
//        case true  => mkLiteral(LongConstant(ctx.getText.toInt), pos(ctx))
//        case false => mkLiteral(IntConstant(ctx.getText.toInt), pos(ctx))
//      }
//    }
//
//    override def visitFloatLit(@NotNull ctx: DcctParser.FloatLitContext): Tree = {
//      val txt = ctx.getText
//      (txt.endsWith("f") || txt.endsWith("F")) match {
//        case true  => mkLiteral(FloatConstant(ctx.getText.toFloat), pos(ctx))
//        case false => mkLiteral(DoubleConstant(ctx.getText.toDouble), pos(ctx))
//      }
//    }
//
//    override def visitCharLit(@NotNull ctx: DcctParser.CharLitContext): Tree = {
//      mkLiteral(CharConstant(ctx.getText.head), pos(ctx))
//    }
//
//    // override def visitStrLit(@NotNull ctx: DcctParser.StrLitContext): Tree = {
//    //   Lit(StringConstant(ctx.getText), pos(ctx))
//    // }
//
//
//    override def visitBoolLit(@NotNull ctx: DcctParser.BoolLitContext): Tree = {
//      mkLiteral(BooleanConstant(ctx.getText.toBoolean), pos(ctx))
//    }
  }
}
object Parser extends Parser
