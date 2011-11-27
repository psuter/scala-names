package ch.epfl.lara.scalanames.features

trait ThrowException extends ContainsTraversalFeature {
  import component._
  import component.global._

  val name = "Method may explicitly throw an exception."
  
  def mkPattern(tree: Tree): Boolean = tree match {
    case t:Throw => true
    case _ => false
  }

}