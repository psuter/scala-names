package ch.epfl.lara.scalanames.features

trait ContainsSelfRecursion extends MethodFeature {
  import component._
  import component.global._

  val name = "Method contains a self-recursion."
  
  def mkRecuTraverser(methodName: String) = new Traverser {
	var foundRecu : Boolean = false

	override def traverse(tree : Tree) : Unit = {
	  if(!foundRecu) {
      	tree match {
      	    case Apply(Select(This(tn),name),args) if(methodName==name.toString())=> foundRecu = true
      	    case _ => super.traverse(tree)
      	}
      }
    }
  }

  def appliesTo(methodDef : MethodDef) : Boolean = {
    val recursifTraverser = mkRecuTraverser(methodDef.name)
    recursifTraverser.traverse(methodDef.d.rhs)
    recursifTraverser.foundRecu
  }
  
  
}