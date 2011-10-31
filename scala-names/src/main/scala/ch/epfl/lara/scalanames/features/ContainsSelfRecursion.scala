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
      	    case Apply(Select(This(tn),name),args) if(methodDef.name==name.toString())=> {
      	     
      	      val params = methodDef.d.symbol.paramss
      	      if(params.isEmpty){
      	        if(args.isEmpty) foundRecu = true
      	        else super.traverse(tree)
      	      } else {
      	        if(verifyArgs(args,params.head)) foundRecu= true
      	        else super.traverse(tree)
      	      }
      	    }
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
  
  private def verifyArgs(args: List[Tree],params: List[Symbol]): Boolean = args match {
    case x :: Nil if(!params.isEmpty)=> args.head.tpe.resultType =:= params.head.tpe
    case x :: xs if(!params.isEmpty)=> args.head.tpe.resultType =:= params.head.tpe && verifyArgs(args.tail,params.tail)
    case Nil => params.isEmpty //no param = true, else wrong function
    case _ => false
  }
  
  
}