package ch.epfl.lara.scalanames.features

trait ContainsInnerMethod extends ContainsTraversalFeature {
  import component._
  import component.global._
  
  val name = "Method body contains method definition"
  
  def mkPattern(tree: Tree): Boolean = tree match {
    case DefDef(mods, name, tparams, vparamss, tpt, rhs) => true
    case _ => false
  }

}