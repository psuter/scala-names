package ch.epfl.lara.scalanames.features

trait ContainsTryCatch extends MethodFeature {
  import component._
  import component.global._

  val name = "Method contains WHILE statement."
  
  def mkTryTraverser = new Traverser {
	var foundTry : Boolean = false

	override def traverse(tree : Tree) : Unit = {
	  if(!foundTry) {
      	tree match {
      	    case t:Try => foundTry = true 
      		case _ => super.traverse(tree) //traverse deeper
      	}
      }
    }
  }

  def appliesTo(methodDef : MethodDef) : Boolean = {
    val tryTraverser = mkTryTraverser
    tryTraverser.traverse(methodDef.d.rhs)
    tryTraverser.foundTry
  }
}