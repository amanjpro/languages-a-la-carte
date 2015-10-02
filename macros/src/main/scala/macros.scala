package ch.usi.inf.l3.sana


// import ch.usi.inf.l3.sana.core.SyntaxComponent
import scala.reflect.macros.blackbox.Context
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

  def defines(id: Any, tpe: String): Boolean = macro MacroImpls.definesImpl
}


object MacroImpls {

  def definesImpl(c: Context)(id: c.Expr[Any],
                              tpe: c.Expr[String]): c.Expr[Boolean] = {
    import c.universe._
    val Literal(Constant(nme: String))   = tpe.tree
    val tnme                             = TypeName(nme)

    val expr = q"""
    $id match {
      case _: $tnme       => true
      case _              => false
    }
    """
    c.Expr[Boolean](expr)
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
