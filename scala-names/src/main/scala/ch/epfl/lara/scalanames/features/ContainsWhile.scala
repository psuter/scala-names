package ch.epfl.lara.scalanames.features

trait ContainsWhile extends MethodFeature {
  import component._
  import component.global._

  val name = "Method contains WHILE statement."
  
  def mkWhileTraverser = new Traverser {
	var foundWhile : Boolean = false

	override def traverse(tree : Tree) : Unit = {
	  if(!foundWhile) {
      	tree match {
      	  //if others items than "while" are translated by compiler into a "LabelDef", need to check LabelDef.name for "while$x"
      	    case l:LabelDef => foundWhile = true 
      		case _ => super.traverse(tree) //traverse deeper
      	}
      }
    }
  }

  def appliesTo(methodDef : MethodDef) : Boolean = {
    val whileTraverser = mkWhileTraverser
    whileTraverser.traverse(methodDef.d.rhs)
    whileTraverser.foundWhile
  }
}