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
      	    //Skip If created by the compiler into the while statement
      	    case LabelDef(_,_,If(cond,then,_)) => {traverse(cond); traverse(then)} 
      	    //Skip If created by the compiler into doWhile statement
      	    case LabelDef(_,_,b@Block(stats,If(cond,_,_))) => super.traverse(Block(stats,cond))
      	    //Catch guard into pattern matching (dropped)
      	    // case CaseDef(_,guard,body) => if(guard.isEmpty) traverse(body) else foundIf = true 
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