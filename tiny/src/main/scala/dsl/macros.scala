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

package ch.usi.inf.l3.sana.tiny


import scala.reflect.macros.blackbox.Context
import scala.annotation.{StaticAnnotation,compileTimeOnly}
import scala.language.experimental.macros


/**
 * Macros are implemented in this module.
 */
object dsl {

  /**
   * a macro to generate a list of phase components.
   *
   * @param trees a coma separated list of the names of the ASTs to generate
   *              instance of phase components for.
   * @param commonName the common name of the components, for example the common
   *                   name between {{{IdentTyperComponen}}} and
   *                   {{{TypeUseTyperComponent}}} is {{{TyperComponent}}}.
   * @param callback the name of the family (delegate) method of the phase family
   *                 that the components become part of.
   * @param exluded A coma separated list of the names of the ASTs that we we want
   *                to exclude from the {{{trees}}} list.
   * @return a list of phase components instances.
   */
  def generateComponents[T, R](trees: String,
    commonName: String, callback: String,
    excluded: String): List[PartialFunction[T, R]] =
    macro MacroImpls.generateComponentsImpl[T, R]

  // def generateFamilies[T <: SyntaxComponent, R](trees: String,
  //   commonName: String, callback: String): List[TransformationComponent[T, R]] =
  //   macro MacroImpls.generateComponentsImpl[T, R]
  //

  // @deprecated("Use @component instead", "0.1")
  // def defines(id: Any, tpe: String): Boolean = macro MacroImpls.definesImpl


  /**
   * component macros to generate the boiler-plate for phase components.
   *
   * This macro takes optionally a parameter. The parameter should be a pair.
   * For more information consult the manual.
   */
  @compileTimeOnly("Enable macro paradise to components")
  class component extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro MacroImpls.componentImpl
  }

}


object MacroImpls {

  def componentImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val inputs = annottees.map(_.tree).toList
    // Luckily Scala compiler is fool enough to accept that all apply
    // methods can have override modifier.
    // We use this foolishness behaviour to help the clients not to
    // distinguish between an overriding component and a component.
    val mods   = Modifiers(Flag.OVERRIDE)
    val expandee = inputs match {
      case (clazz: ClassDef) :: Nil =>

        val (variables, head): (List[ValDef], Option[ValDef]) =
            c.prefix.tree match {
              case Apply(_, xs) if xs.size >= 2 =>
                val vals = xs.zipWithIndex.map((x) => {
                  val tree  = x._1
                  val index = TermName("_" + (x._2 + 1))
                  q"val ${TermName(tree.toString)} = p.$index"
                })
                (vals, vals.headOption)
              case Apply(_, xs) if xs.size == 1 =>
                c.abort(c.enclosingPosition,
                  "components for tuple should be used for pairs")
                (Nil, None)
              case _ =>
                (Nil, None)
            }

        // We are sure it exists, otherwise we wouldn't have been here
        // val head = variables.head

        val impl = clazz.impl.body.flatMap { mthd =>
          mthd match {
            case pf@Function(List(param), rhs) =>
              val app =
              q"""
                $mods def apply(p: Input): Output = {
                  ..$variables
                  ${head match {
                      case Some(v) =>
                        q"""${v.name} match {
                          case a: ${param.tpt} => ${pf}.apply(a)
                        }"""
                      case _   =>
                        q"""p match {
                          case a: ${param.tpt} => ${pf}.apply(a)
                        }
                        """
                      }
                    }
                }
              """
              val isDefinedAt =
              q"""
                $mods def isDefinedAt(p: Input): Boolean =
                  ${head match {
                      case Some(v) =>
                        q"""p match {
                          case (a: ${param.tpt}, _)     => true
                          case _                        => false
                        }"""
                      case _   =>
                        q"""p match {
                          case a: ${param.tpt}          => true
                          case _                        => false
                        }"""
                  }
                }
              """
              List(app, isDefinedAt)
            case m                             => List(m)
          }
        }.filter { mthd =>
          mthd match {
            case mt: DefDef    =>
              mt.name != TermName("$init$")
            case _             =>
              true
          }
        }
        q"""
        ${clazz.mods} trait ${clazz.name} extends ..${clazz.impl.parents} {
          ..$impl
        }"""
      case _ =>
        c.abort(c.enclosingPosition,
          "Only traits/classes can become component")
        EmptyTree
    }
    c.Expr[Any](expandee)
  }


  def generateComponentsImpl[T : c.WeakTypeTag,
      R: c.WeakTypeTag](c: Context)
      (trees: c.Expr[String], commonName: c.Expr[String],
      callback: c.Expr[String],
      excluded: c.Expr[String]): c.Expr[List[PartialFunction[T, R]]] = {

    import c.universe._

    val Literal(Constant(commonNameString: String))   = commonName.tree
    val Literal(Constant(callbackString: String))     = callback.tree
    val treeList                                      = {
      val duplicated = trees.tree.duplicate
      val constant   = c.Expr[String](c.untypecheck(duplicated))
      c.eval[String](constant).split(",").map(_.trim)
    }.toList
    val excludedTrees                                 = {
      val duplicated = excluded.tree.duplicate
      val constant   = c.Expr[String](c.untypecheck(duplicated))
      c.eval[String](constant).split(",").map(_.trim)
    }.toList

    val components = for {
      tree <- treeList if !excludedTrees.contains(tree)
    } yield {
      val cbName    = TermName(callbackString)
      val clazzName = TypeName(tree + commonNameString)
      q"""
        new $clazzName {
          def compiler = self.compiler
          def $cbName = self.$cbName
        }
      """
    }


    val expr = q"$components"


    c.Expr[List[PartialFunction[T, R]]](expr)
  }
}
