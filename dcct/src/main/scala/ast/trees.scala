package ch.usi.inf.l3.sana.dcct.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import tiny.ast._
import tiny.source.Position
import tiny.types.Type
import tiny.names.Name
import tiny.symbols.Symbol
import tiny.modifiers.Flags
import primj.types._
/**
 * Some things to keep in mind when defining trees:
 * 1) This framework is all about reusing trees from other modules as much as possible.
 * 2) As far as I have understood, no need to be too type specific or type safe when defining 
 * trees, so we do not lose flexibility (not convinced). If needed, check the shapes of trees 
 * at a compilation phase. Let the parser rule out wrong shapes, and handle semantics at 
 * code generation.
 * 3) How to know whether to name a tree as x or xApi? and xApi is a tree that is meant to be instantiated.
 * while x is not. Note however, that you are supposed to provide factories to instantiate trees.
 * 4) How to know whether to create concrete tree fields, or open-class style fields? generally, 
 * things that have to do with the actual shape of the tree are concrete fiendsl, other things like the 
 * symbol are an open-class style field. Again notice that you are never supposed to manually manipulate the map.
 * but you have to do it throught implicit style conversions.  
 * 
 */
/**
 * This trait is used to restrict possible DefTrees that
 * can be a part of a Dcct program.
 * 
 * TODO might want to remove it later and just use DefTree, relying on my parser to generate
 * the correct trees used only by my language. Actually I think I will try that now!
 */
//trait DcctDefTree extends DefTree {
//}

/**
 * Program
 * The root of my compilation tree. Will take from primj
 * a program starts with schema delarations, that is, entities 
 * and arrays, then a list of functions that operate on the schema.
 * 
 * Example: 
 * 
 * // All schema declarations at the beginning of the program, used to 
 * create a data store schema.
 * entity Customer {
 *   address: CString
 * }
 * 
 * // functions that operate on the schema.
 * function createCusomer() {
 *  ... 
 * }
 * 
 * Used tree: sana.primj.ast.ProgramApi 
 */


/**
 * Repesents an entity definition of a scheme. Entities appear at the beginning of the program. 
 * propoerties are attached to entities, and point only to cloud types. In this language, each 
 * property is at an integer index. 
 * examples:
 * 
 * entity Customer {
 *   name: CString // propoerty, this is syntax sugar.
 * }
 * 
 * entity Order(customer: Customer) { // this is a weak entity, depends of customer.
 *   time: CTime
 * }
 * 
 * an entity is like a class, except that it does not contain methods, hence we use the class
 * from ooj to utilize constructors and similar features.
 * 
 * TODO I cannot find the constructor in ClassDef!
 * I believe it has to be added during compilation, after assigning a symbol or something.
 * 
 * 
 */

/**
 * Arrays are special entities, they always exist with some default value, and have 
 * explicit indices. Their semantics eliminate contention on indices. 
 * 
 * In theory, I could use the class definition trees to implement them, but it seems too awkward
 * new and stuff will not work. So I will just define a new tree for them.
 * 
 * TODO consult Amanj about using the ClassDef tree for arrays.
 */
trait ArrayDefApi extends NamedTree {
  def name: Name
  def indices: List[TypeUseApi]
  def properties: List[Expr] // TODO not sure about the type here.
  
  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = indices.foldLeft(z)((z, y) => {
      y.bottomUp(z)(f)
    })
    val r2 = properties.foldLeft(z)((z, y) => {
      y.bottomUp(z)(f)
    })
    
    f(r2, this) // TODO not sure if r2 here is correct.
  }
}

/**
 * TODO how to validate declared types? how to attach tpt to trees? 
 * Binding happens at parser and validation at typer. Look at examples from primJ
 */

/**
 * Here I scanned my grammar and tried to figure out what to do with all language components.
 * 
 * 1) Cloud, index and expression types assigned at parser, checked at typer and exist 
 * at the runtime, in the future....
 * 
 * 2) I do not see the point of having this weird property sugar and de-sugar. I think it simplifies the grammar
 * but nothing more. I will just have them as as part of my trees I think.
 * 
 * 3) The update and query operatiosn are just applies, and are a part of the runtimes. Maybe I provide
 * language constructs in the future to actually define them.
 * 
 * 4) delete, entries and all are treated as apply.
 * 
 */

/**
 * This construct is used to query entities and lists. 
 * Example:
 * 
 * foreach order in all Order
 * where order.Customer == customer
 * { 'do something here' } 
 * 
 */
trait ForEachApi extends Expr {
  def inits: List[Tree]
  def allOrEntries: primj.ast.ApplyApi
  def cond: Expr
  def body: Expr

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = inits.foldLeft(z)((z, y) => {
      y.bottomUp(z)(f)
    })
    val r2 = allOrEntries.bottomUp(z)(f)
    val r3 = cond.bottomUp(r1)(f)
    val r4 = body.bottomUp(r2)(f)

    f(r4, this)
  }
}

/**
 * entity and array filters are 'all' and 'entries'. I will implement them 
 * using Apply from primj.
 * 
 * TODO I am not sure if I can use methods with apply for everything. I could have some sort of Env
 * object and call methods on that. I do not really like it though but whatever. 
 */

/**
 * 'all' returns a list of all instances of an entity
 * 
 */

/**
 * 'entries p' returns a subset of elements in an array, where 
 * 'p' is set to a value other than the default.
 */

/**
 * flush and yeild are also implemented using Apply! 
 */

protected[ast] class ForEach(val inits: List[Tree], val allOrEntries: primj.ast.ApplyApi,
  val cond: Expr, val body: Expr) extends ForEachApi {
  override def toString: String =
    s"For($inits, $cond, $allOrEntries, $body)"
}

protected[ast] class ArrayDef(val name: Name, val indices: List[TypeUseApi], val properties: List[Expr]) extends ArrayDefApi {
  override def toString: String =
    s"Array$indices,$properties)"
} 

