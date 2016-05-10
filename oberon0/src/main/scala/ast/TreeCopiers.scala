package ch.usi.inf.l3.sana.oberon0.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.arrayj
import sana.ooj

import tiny.ast._
import tiny.ast.Implicits._
import primj.ast.{BlockApi}
import tiny.names.Name
import tiny.symbols.Symbol
import tiny.source.Position
import arrayj.ast.{TreeCopiers => ATreeCopiers, _}
import ooj.ast.{TreeCopiers => OTreeCopiers, _}


trait TreeCopiers extends primj.ast.TreeCopiers {

  def copyModuleDef(template: ModuleDefApi)(name: Name = template.name,
    declarations: List[DefTree] = template.declarations,
    block: Option[BlockApi] = template.block): ModuleDefApi = {
    val res = TreeFactories.mkModuleDef(name, declarations, block)
    copyProperties(template, res)
    res
  }


  def copyTypeDef(template: TypeDefApi)(name: Name = template.name,
    tpt: Tree = template.tpt): TypeDefApi = {

    val res = TreeFactories.mkTypeDef(name, tpt)
    copyProperties(template, res)
    res
  }

  def copyArrayTypeUse(template: ArrayTypeUseApi)(
      tpt: UseTree = template.tpt,
      size: Expr  = template.size): ArrayTypeUseApi = {
    val res = TreeFactories.mkArrayTypeUse(tpt, size)
    copyProperties(template, res)
    res
  }

  def copyRecordDef(template: ClassDefApi)(
      body: TemplateApi = template.body): ClassDefApi =
    OTreeCopiers.copyClassDef(template)(template.mods,
      template.name, template.parents, body)

  def copyTemplate(template: TemplateApi)(
      members: List[Tree] = template.members): TemplateApi =
    OTreeCopiers.copyTemplate(template)(members)

  def copySelect(template: SelectApi)(qual: Tree = template.qual,
    tree: SimpleUseTree = template.tree): SelectApi =
    OTreeCopiers.copySelect(template)(qual, tree)


  def copyArrayAccess(template: ArrayAccessApi)(
    array: Expr = template.array,
    index: Expr = template.index): ArrayAccessApi =
    ATreeCopiers.copyArrayAccess(template)(array, index)



}

object TreeCopiers extends TreeCopiers
