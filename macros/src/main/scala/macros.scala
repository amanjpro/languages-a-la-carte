package ch.usi.inf.l3.sana


// import ch.usi.inf.l3.sana.core.SyntaxComponent
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


  @compileTimeOnly("Enable macro paradise to expand macro annotations")
  class component extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro MacroImpls.componentsImpl
  }
}


object MacroImpls {


  def componentsImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val inputs = annottees.map(_.tree).toList
    val expandee = inputs match {
      case (clazz: ClassDef) :: Nil =>
        val impl = clazz.impl.body.flatMap { mthd =>
          mthd match {
            case pf@Function(List(param), rhs) =>
              val app = q"""
                def apply(p: Input): Output = p match {
                  case p: ${param.tpt} => ${pf}.apply(p)
                }
              """
              val isDefinedAt = q"""
                def isDefinedAt(p: Input): Boolean = p match {
                  case p: ${param.tpt} => true
                  case _               => false
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
    }
    c.Expr[Any](expandee)
  }

  // @deprecated("Use @component instead", "0.1")
  // def definesImpl(c: Context)(id: c.Expr[Any],
  //                             tpe: c.Expr[String]): c.Expr[Boolean] = {
  //   import c.universe._
  //   val Literal(Constant(nme: String))   = tpe.tree
  //   val tnme                             = TypeName(nme)
  //
  //   val expr = q"""
  //   $id match {
  //     case _: $tnme       => true
  //     case _              => false
  //   }
  //   """
  //   c.Expr[Boolean](expr)
  // }

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
