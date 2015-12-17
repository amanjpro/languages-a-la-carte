package ch.usi.inf.l3.sana.tiny.symbols

import ch.usi.inf.l3.sana
import sana.tiny.types.Type
import sana.tiny.modifiers.Flags
import sana.tiny.names.Name


trait Symbol {
  var name: Name
  var mods: Flags
  var tpe: Option[Type]
  var owner: Option[Symbol]

  protected[this] var decls: List[Symbol] = Nil

  def declarations: List[Symbol] = decls


  def declare(symbol: Symbol): Unit = decls = symbol :: decls

  def delete(symbol: Symbol): Unit = decls = decls.filter(_ != symbol)

  def directlyDefines(symbol: Symbol,
    p: Symbol => Boolean): Boolean = decls.exists { s =>
      s == symbol && p(s)
    }

  def getDirectlyDefinedSymbols(name: Name,
      p: Symbol => Boolean): List[Symbol] =
    decls.filter { sym =>
      sym.name == name && p(sym)
    }


  // Handling scoping, does this defines a symbol? If not see if
  // the owner defines it
  def defines(symbol: Symbol): Boolean =
    defines(symbol, _ => true)
  def defines(symbol: Symbol, p: Symbol => Boolean): Boolean =
    decls.exists(s => s == symbol && p(s)) || owner.map { sym =>
      sym.defines(symbol, p)
    }.getOrElse(false)


  // Handling scoping, does this defines a name with a predicate? If
  // not see if the owner defines it
  def getSymbol(name: Name, p: Symbol => Boolean): Option[Symbol] = {
    val thisSym = decls.find { sym =>
      sym.name == name && p(sym)
    }
    thisSym match {
      case None =>
        owner.flatMap { sym =>
          sym.getSymbol(name, p)
        }
      case _    => thisSym
    }
  }

}


trait TermSymbol extends Symbol
trait TypeSymbol extends Symbol
