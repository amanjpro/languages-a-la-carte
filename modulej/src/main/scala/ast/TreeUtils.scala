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



import tiny.ast.{SimpleUseTree, UseTree, Tree}
import modulej.ast.Implicits._
import ooj.ast.PackageDefApi
import ooj.names.StdNames.DEFAULT_PACKAGE_NAME
import tiny.ast.{TypeUseApi, IdentApi}
import modulej.ast.TreeExtractors._
import tiny.names.{Name, StdNames}
import tiny.source.Position
import ooj.symbols.PackageSymbol

trait TreeUtils extends ppj.ast.TreeUtils {
  def toFullyQualifiedTree(sym: PackageSymbol,
          pos: Option[Position]): UseTree = {
    fromQualifiedString(sym.qualifiedName)
  }

  def fromQualifiedString(name: String): UseTree = {
    def helper(names: List[String]): UseTree = names match {
      case (n::Nil)                          =>
        TreeFactories.mkIdent(Name(n))
      case (n::ns)                           =>
        val tree = TreeFactories.mkIdent(Name(n))
        val qual = helper(ns)
        TreeFactories.mkSelect(qual, tree)
      case _                                 =>
        ???
    }
    helper(name.split("[.]").toList.reverse)
  }

  def toQualifiedString(use: UseTree): String = use match {
    case Select(qual: UseTree, t)           =>
      s"${toQualifiedString(qual)}.${t.name.asString}"
    case id: IdentApi                       =>
      id.name.asString
    case tuse: TypeUseApi                   =>
      tuse.name.asString
    case _                                  =>
      ""
  }

  def attachQualifiedNameAttribute(use: UseTree): Unit = {
    def helper(use: UseTree): Unit = {
      use match {
        case s: SimpleUseTree                =>
          s.fullyQualifiedName = s.name.asString
        case Select(qual: UseTree, tree)     =>
          helper(qual)
          qual.fullyQualifiedName match {
            case None                        =>
              use.fullyQualifiedName      = tree.name.asString
              tree.fullyQualifiedName     = tree.name.asString
            case Some(n)                     =>
              use.fullyQualifiedName      = s"$n.${tree.name.asString}"
              tree.fullyQualifiedName     = s"$n.${tree.name.asString}"
          }
        case _                               =>
          ()
      }
    }
    use.fullyQualifiedName match {
      case None                  => helper(use)
      case Some(fq)              => ()
    }
  }

  /** Takes a list of package names and turns them into a hierarchy of
    * packages. The head of the list is the outer most package,
    * and the tail is the inner ones.
    */
  def toPackage(names: List[String]): PackageDefApi =
    toPackageAux(names.reverse)

  protected def toPackageAux(names: List[String]): PackageDefApi =
    names match {
      case (x::xs)                  =>
        TreeFactories.mkPackageDef(xs.map(Name(_)).reverse, Name(x), Nil)
      case _                         =>
        TreeFactories.mkPackageDef(Nil, DEFAULT_PACKAGE_NAME, Nil)
    }
}

object TreeUtils extends TreeUtils
