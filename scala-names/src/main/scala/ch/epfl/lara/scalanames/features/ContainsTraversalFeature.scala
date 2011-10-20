package ch.epfl.lara.scalanames.features

trait ContainsTraversalFeature extends MethodFeature {
  import component._
  import component.global._
    
  def mkTraverser = new Traverser {
	var foundFeature : Boolean = false

	//If a look ahead is needed on the ast to answer if the feature is contained,
	//this implementation will fail
	override def traverse(tree : Tree) : Unit = {
	  if(!foundFeature) {
      	if(mkPattern(tree)) foundFeature = true
      	else super.traverse(tree)
      }
    }
  }

  def appliesTo(methodDef : MethodDef) : Boolean = {
    val featureTraverser = mkTraverser
    featureTraverser.traverse(methodDef.d.rhs)
    featureTraverser.foundFeature
  }
  
  def mkPattern(tree: Tree) : Boolean
}