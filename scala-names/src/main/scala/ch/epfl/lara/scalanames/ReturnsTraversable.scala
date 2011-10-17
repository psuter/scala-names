package ch.epfl.lara.scalanames

import scala.tools.nsc.Global

trait ReturnsTraversable extends MethodFeature {
  import component._
  import component.global._
 
  // Recovers the __symbol__ for the Traversable trait
  private lazy val traversableTraitSymbol : Symbol = global.definitions.getClass("scala.collection.Traversable")
  
  val name = "Method returns a collection (Traversable)."
  
  def appliesTo(md : MethodDef) : Boolean = { 
    md.rettype.baseType(traversableTraitSymbol) != NoType
  }
}