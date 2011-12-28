package ch.epfl.lara.scalanames.features

trait ReturnsTraversable extends ReturnSubtypeOf{
  import component._
  import component.global._
  
  // Recovers the __symbol__ for the Traversable trait
  lazy val traitSymbol = global.definitions.getClass("scala.collection.Traversable");
  
  override lazy val name = "Method returns a collection (Traversable)."

}