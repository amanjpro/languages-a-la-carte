package ch.usi.inf.l3.sana.dcct.codegenerator

import ch.usi.inf.l3.sana
import sana.dcct
import sana.ooj
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj

import sana.core.TransformationComponent
import sana.dsl._
import tiny.ast._
import tiny.symbols._
import tiny.ast.Implicits._
import tiny.source.Position
import calcj.ast._
import calcj.ast.operators.{Inc, Dec}
import tiny.errors.ErrorReporting.{error,warning}
import primj.ast.{MethodDefApi => _, TreeUtils => _, ProgramApi => _, _}
import primj.typechecker.ShapeCheckerComponent
import ooj.symbols._
import ooj.modifiers.Ops._
import ooj.errors.ErrorCodes._
import ooj.ast._
import ooj.names.StdNames._
import ooj.ast.TreeExtractors._


trait CodeGenComponent extends TransformationComponent[Tree, String] {
  def codegen: Tree => String
}


@component 
trait ProgramCodeGenComponent  extends CodeGenComponent{
  (prg: primj.ast.ProgramApi) => {
    val dataDefScript = prg.members.foldLeft("")((c,x) => c + codegen(x) + "\n")
    
    s"DROP KEYSPACE IF EXISTS $prg.sourceName \n" +
    s"CREATE KEYSPACE $prg.sourceName \n" +
    "WITH replication = {'class': 'SimpleStrategy', 'replication_factor' : 3}; \n" + 
    s"USE $prg.sourceName; \n" + 
    dataDefScript
  }
}


@component 
trait EntityCodeGenComponent  extends CodeGenComponent{
  (entity: ooj.ast.ClassDefApi) => {
    val entityDef = entity.body.members.foldLeft(s"CREATE TABLE $entity.name (")( (c, x) => {
      val field = x.asInstanceOf[ValDefApi]
      val fName = field.name
      val fType = field.tpt.asInstanceOf[TypeTree].name
      fName + " " + fType + "\n"
    })
  entityDef + " )"
  }
}