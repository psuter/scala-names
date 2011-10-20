package ch.epfl.lara.scalanames.features

trait ContainsIf extends MethodFeature {
  import component._
  import component.global._

  val name = "Method contains IF statement."
  
  def mkIfTraverser = new Traverser {
	var foundIf : Boolean = false

	override def traverse(tree : Tree) : Unit = {
	  if(!foundIf) {
      	tree match {
      	    case LabelDef(_,_,If(cond,then,_)) => {traverse(cond); traverse(then)} //Skip If into while condition
      		case i : If => foundIf = true
      		case _ => super.traverse(tree) //traverse deeper
      	}
      }
    }
  }

  def appliesTo(methodDef : MethodDef) : Boolean = {
    val ifTraverser = mkIfTraverser
    ifTraverser.traverse(methodDef.d.rhs)
    ifTraverser.foundIf
  }
}