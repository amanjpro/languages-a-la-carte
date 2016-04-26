package ch.usi.inf.l3.sana.robustj.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import sana.ooj
import sana.robustj

import tiny.modifiers.Ops._
import primj.ast.{MethodDefApi => PMethodDefApi}
import ooj.ast.{MethodDefApi => OMethodDefApi}
import robustj.ast.{MethodDefApi => RMethodDefApi}

trait TreeUpgraders {
  def upgradeMethodDef(mthd: PMethodDefApi): RMethodDefApi = mthd match {
    case mthd: RMethodDefApi             => mthd
    case mthd: OMethodDefApi             =>
      val res         = TreeFactories.mkMethodDef(mthd.mods,
                                                  mthd.ret,
                                                  mthd.name,
                                                  mthd.params,
                                                  Nil,
                                                  mthd.body)
      res.attributes = mthd.attributes
      res
    case _                               =>
      val res         = TreeFactories.mkMethodDef(noflags,
                                                  mthd.ret,
                                                  mthd.name,
                                                  mthd.params,
                                                  Nil,
                                                  mthd.body)
      res.attributes = mthd.attributes
      res
  }
}


object TreeUpgraders extends TreeUpgraders

