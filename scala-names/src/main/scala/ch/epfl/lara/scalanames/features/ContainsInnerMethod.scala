package ch.epfl.lara.scalanames.features

trait ContainsInnerMethod extends MethodFeature {
  import component._
  import component.global._
  
  val name = "Method body contains method definition"
  
   def mkDefTraverser = new Traverser {
	var foundDef : Boolean = false

	override def traverse(tree : Tree) : Unit = {
	  if(!foundDef) {
      	tree match {
      	  case cd : ClassDef => 
      	  case dd : DefDef => foundDef = true
      	  //traverse deeper
      	  case _ => super.traverse(tree)
      	}
      }
    }
  }

  def appliesTo(methodDef : MethodDef) : Boolean = {
    val defTraverser = mkDefTraverser
    defTraverser.traverse(methodDef.d.rhs)
    defTraverser.foundDef
  }

}