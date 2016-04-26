package ch.usi.inf.l3.sana.ooj.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import sana.ooj

import tiny.modifiers.Ops._
import primj.ast.{MethodDefApi => PMethodDefApi}
import ooj.ast.{MethodDefApi => OMethodDefApi}

trait TreeUpgraders {
  def upgradeMethodDef(mthd: PMethodDefApi): OMethodDefApi = mthd match {
    case mthd: OMethodDefApi             => mthd
    case _                               =>
      val res         = TreeFactories.mkMethodDef(noflags,
                                                  mthd.ret,
                                                  mthd.name,
                                                  mthd.params,
                                                  mthd.body)
      res.attributes = mthd.attributes
      res
  }
}


object TreeUpgraders extends TreeUpgraders

