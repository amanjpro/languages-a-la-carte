package ch.usi.inf.l3.sana.modulej.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import sana.ooj
import sana.robustj
import sana.modulej

import tiny.modifiers.Ops._
import ooj.ast.{CompilationUnitApi => OCompilationUnitApi}
import modulej.ast.{CompilationUnitApi => MCompilationUnitApi}

trait TreeUpgraders extends robustj.ast.TreeUpgraders {
  def upgradeCompilationUnit(
    cunit: OCompilationUnitApi): MCompilationUnitApi =
    cunit match {
      case cunit: MCompilationUnitApi         => cunit
      case cunit: OCompilationUnitApi         =>
        val res = TreeFactories.mkCompilationUnit(Nil,
          cunit.module, cunit.sourceName, cunit.sourcePath)
        res.attributes = cunit.attributes
        res
    }
}


object TreeUpgraders extends TreeUpgraders

