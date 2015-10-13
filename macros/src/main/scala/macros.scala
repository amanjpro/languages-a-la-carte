package ch.usi.inf.l3.sana


import scala.reflect.macros.blackbox.Context
import scala.annotation.{StaticAnnotation,compileTimeOnly}
import scala.language.experimental.macros


object dsl {
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
          def $cbName = self.$cbName
        }
      """
    }


    val expr = q"$components"


    c.Expr[List[PartialFunction[T, R]]](expr)
  }
}
