package ch.epfl.lara.scalanames.features

trait ContainsPatternMatching extends ContainsTraversalFeature {
  import component._
  import component.global._

  val name = "Method contains Pattern matching."
  
  def mkPattern(tree: Tree): Boolean = tree match {
    case m:Match => true
    case _ => false
  }
}