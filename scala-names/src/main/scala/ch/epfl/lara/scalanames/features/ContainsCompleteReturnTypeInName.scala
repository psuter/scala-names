package ch.epfl.lara.scalanames.features

trait ContainsCompleteReturnTypeInName extends ContainsPartialReturnTypeInName {
  import component._
  import component.global._
  
  override lazy val name = "Method return type is completly contained into methodName."
    
      
  override def appliesTo(methodDef: MethodDef): Boolean = {
    
	val cp = reconstructAcronym(methodDef.name)
    	
	if(cp.length>0){
	  val upperCp = firstLetterToUpperCase(cp)
	  val splitTypeName = reconstructAcronym(resultType(methodDef.rettype))
	  
	  splitTypeName.length match {
	    case 0 => false
	    case _ => containsReturnTypeInName(upperCp,splitTypeName)
	  }
	
	} else false	
  }
  
  def containsReturnTypeInName(mn: List[String], tn:List[String]): Boolean = {
    
    def apply(mn: List[String], tn: List[String]): Boolean = mn match {
	  case x :: Nil if(===(x, tn.head)) => tn.length == 1
	  case x :: xs  if(===(x, tn.head)) => if(tn.length==1) true else checkNext(xs,tn.tail) || apply(xs,tn)
	  case x :: xs  => apply(xs,tn)
	  case _ => false
	}
	
	def checkNext(mn: List[String], tn: List[String]): Boolean = mn match {
	  case x :: Nil if(===(x, tn.head))=> tn.length == 1
	  case x :: xs  if(===(x, tn.head))=> if(tn.length==1) true else checkNext(xs,tn.tail)  
	  case _ => false
	}
	
	//Often used abreviation
	def ===(word:String,ttype:String):Boolean = word match {
	  case "Str" | "str" if(ttype=="String")=> true 
	  case "Bool" | "bool" if(ttype=="Boolean")=> true
	  case "Int" | "int" if(ttype=="Integer")=> true
	  case _ => word==ttype
	}
	
	apply(mn,tn)
  }

}