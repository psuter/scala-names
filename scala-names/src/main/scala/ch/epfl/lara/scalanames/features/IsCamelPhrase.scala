package ch.epfl.lara.scalanames.features


trait IsCamelPhrase extends MethodFeature {
  import component._
  import component.global._
  
  lazy val name = "Method name is a camel phrase."

  def appliesTo(methodDef: MethodDef): Boolean = {
    val ws = splitWord(methodDef.name)
    ws.length > 1
  }
  
  //Split camel phrase
  //return an empty list if contains only special characters  
  def splitWord(methodName: String): List[String] = {
  
	def apply(name: String, current: String, res: List[String]): List[String] = {
    
	  def cur = if(current=="")res else res:::List(current)
    
	  if (name.isEmpty) cur
		else {
		  val head = name.head
		  if (head.isLetter){ 											//a letter
			if(head.isUpperCase) apply(name.tail,head.toString,cur) 	//uppercase letter
			else apply(name.tail,current+head,res)  			    	//lowercase letter
		  } else if (head.isDigit) apply(name.tail,head.toString,cur)	//a digit      
		  else apply(name.tail,"",cur)									//a special character
		}
	}
	
	apply(methodName,"",List())
  }
}