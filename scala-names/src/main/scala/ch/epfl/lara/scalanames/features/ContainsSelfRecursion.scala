package ch.epfl.lara.scalanames.features

trait ContainsSelfRecursion extends MethodFeature {
  import component._
  import component.global._

  val name = "Method contains a self-recursion."
  
  def mkRecuTraverser(methodDef: MethodDef) = new Traverser {
	var foundRecu : Boolean = false

	override def traverse(tree : Tree) : Unit = {
	  if(!foundRecu) {
      	tree match {
      	    case a@Apply(Select(This(_),_),_) if(methodDef.d.symbol==a.symbol)=> foundRecu = true
      	    case _ => super.traverse(tree)
      	}
      }
    }
  }

  def appliesTo(methodDef : MethodDef) : Boolean = {
    val recursifTraverser = mkRecuTraverser(methodDef)
    recursifTraverser.traverse(methodDef.d.rhs)
    recursifTraverser.foundRecu
  }
}