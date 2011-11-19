package ch.epfl.lara.scalanames.features

trait ContainsReturnTypeInName extends ContainsAcronym {
  import component._
  import component.global._
  
  override val name = "Method return type is contained into methodName."
    
      
  override def appliesTo(methodDef: MethodDef): Boolean = {
    
	val cp = reconstructAcronym(splitWord(methodDef.name,"",List()),"",List())
    
	val typeName = methodDef.d.symbol.tpe.resultType.toString()
	
	containsReturnTypeInName(cp,splitWord(removeSpecialCharOfType(typeName),"",List()))
  }
  
  def removeSpecialCharOfType(tn: String): String = {
    //remove dot
	val stn = if(tn.contains(".")) tn.split("\\u002E").last else tn
	//remove brackets
	val stn2 = if(stn.contains("[")) (stn.split("\\u005B")).head else stn
	//remove parenthesis
	val stn3 = if(stn2.contains(")")) stn2.split("\\u0029").last else stn2
	
	stn3
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