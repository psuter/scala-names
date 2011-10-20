package ch.epfl.lara.scalanames.features

trait ContainsWhile extends ContainsTraversalFeature {
  import component._
  import component.global._

  val name = "Method contains WHILE statement."
  
  def mkPattern(tree: Tree): Boolean = tree match {
    case l:LabelDef => true
    case _ => false
  }
}