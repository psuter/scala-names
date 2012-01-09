package ch.epfl.lara.scalanames.features

trait ContainsLazyVal extends MethodFeature {
  import component._
  import component.global._
  
   val name = "Method contains lazy val."
  
  def mkLazyTraverser = new Traverser {
	var found: Boolean = false

	override def traverse(tree : Tree) : Unit = {
	  if(!found) {
      	tree match {
      	  case x if (x.symbol != null && x.symbol.isLazy) => found = true
      	  case _ => super.traverse(tree)
      	}
      }
    }
  }

  def appliesTo(methodDef : MethodDef) : Boolean = {
    val lazyTraverser = mkLazyTraverser
    lazyTraverser.traverse(methodDef.d.rhs)
    lazyTraverser.found
  }
}