package ch.usi.inf.l3.sana.dcct.ast.augmenters

import ch.usi.inf.l3.sana
import sana.tiny.ast.TypeUseApi
import sana.tiny.names.Name

import sana.tiny.ast.TypeUseApi

trait AugmentedTypeUseApi {

  def tree: TypeUseApi

  def consistencyAnnotation: Option[Name] =
    tree.attributes.get('consistencyAnnotation).map(_.asInstanceOf[Name])
     
  def consistencyAnnotation_=(name: Name): Unit =
    tree.attributes = tree.attributes + ('consistencyAnnotation -> name)

   def consistencyRegion: Option[Name] =
    tree.attributes.get('consistencyRegion).map(_.asInstanceOf[Name])
   
  def consistencyRegion_=(name: Name): Unit =
    tree.attributes = tree.attributes + ('consistencyRegion -> name)

  def show: String = {
    s"[THE AUG TREE]  ${consistencyAnnotation.toString} ${consistencyRegion.toString}  ${tree.toString}" 
  }

}

