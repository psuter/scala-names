package ch.epfl.lara.scalanames.features

trait ContainsTryCatch extends ContainsTraversalFeature {
  import component._
  import component.global._

  val name = "Method contains TRY/CATCH statement."
    
  def mkPattern(tree: Tree): Boolean = tree match {
    case t:Try => true
    case _ => false
  }
}