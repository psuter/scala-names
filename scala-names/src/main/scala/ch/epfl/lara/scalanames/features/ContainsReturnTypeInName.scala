package ch.epfl.lara.scalanames.features

trait ContainsReturnTypeInName extends ContainsAcronym {
  import component._
  import component.global._
  
  override val name = "Method return type is contained into methodName."
    
      
  override def appliesTo(methodDef: MethodDef): Boolean = {
    
	val cp = reconstructAcronym(splitWord(methodDef.name,"",List()),"",List())
    	
	val typeName = (unfoldMethodType(methodDef.d.symbol.tpe.resultType)).typeConstructor
	
	//TODO name class, puis appliquer une des méthod pr retriver le nom + à la place de :plus
	//TODO K-mean, PCA
	
	//println(typeName+": "+removeSpecialCharOfType(typeName.toString()))
	
	val splitType = splitWord(removeSpecialCharOfType(typeName.toString()),"",List())
	
	if(splitType.isEmpty) false //May be wrong for type with special character, like _
	else containsReturnTypeInName(cp,splitType)
	
  }
  
  def unfoldMethodType(tt: Type):Type = tt match {
    case MethodType(ls,rt) => unfoldMethodType(rt)
	case x => x
  }
  
  def removeSpecialCharOfType(tn: String): String = {
    //remove java package dot
	if(tn.contains(".")) tn.split("\\u002E").last else tn
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