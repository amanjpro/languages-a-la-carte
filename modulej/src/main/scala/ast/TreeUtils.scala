package ch.usi.inf.l3.sana.modulej.ast


import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.brokenj
import sana.ooj
import sana.arrayj
import sana.arrooj
import sana.robustj
import sana.dynj
import sana.ppj
import sana.modulej



import tiny.ast.UseTree
import tiny.ast.Tree
import tiny.names.{Name, StdNames}
import tiny.source.Position
import ooj.symbols.PackageSymbol

trait TreeUtils extends ppj.ast.TreeUtils {
  def toFullyQualifiedTree(sym: PackageSymbol,
          pos: Option[Position]): UseTree = {
    def toQualifiedTree(names: List[Name]): UseTree = names match {
      case (x::Nil)                       =>
        TreeFactories.mkIdent(x, pos)
      case (x::xs)                        =>
        val id   = TreeFactories.mkIdent(x, pos)
        val qual = toQualifiedTree(xs)
        TreeFactories.mkSelect(qual, id, pos)
      case Nil                            =>
        // INFO: This should never happen
        TreeFactories.mkIdent(StdNames.noname, pos)
    }
    toQualifiedTree(sym.qualifiedNameAsList.reverse)
  }
}

object TreeUtils extends TreeUtils
