package ch.epfl.lara.scalanames.features

trait ContainsAcronym extends MethodFeature {
  import component._
  import component.global._
  
  override lazy val name = "Method name contains an acronym"
    
  /**
   * Acronym is defined as list of capital letter and digit and must start with a letter  
   */
    
  override def appliesTo(methodDef: MethodDef): Boolean = {
    val ws = splitWord(methodDef.name)
    if(ws.length > 1){
      val cp = reconstructAcronym(ws)      
      cp.filter(isAcronym).size > 0
    }
    else false //only 1 word, not an acronym
  }

  //Take the splited camel phrase as input and return a camel phrase with reconstructed acronyms
  private def reconstructAcronym(ws: List[String]): List[String] = {
    def apply(ss: List[String], current: String, res:List[String]): List[String] = ss match {
    	case Nil => ss											//Empty list, should not happen
    	case x :: Nil if(current=="")=> res:::List(x)			//At the end of a camel phrase. Previous word is not an acronym, this one either (or one letter)
    	case x :: Nil if(x.length()==1)=> res:::List(current+x) //At the end of a camel phrase. Last word is an acronym
    	case x :: Nil => res:::List(current,x)					//At the end of a camel phrase. Last word is not an acronym but previous is
    	case x :: xs if(x.length()==1)=> apply(xs,current+x,res) //inside camel phrase. We discover a new acronym or continue one
    	case x :: xs if(current != "" && x.length()!=1)=> apply(xs,"",res:::List(current,x)) //inside camel phrase. End of an acronym
    	case x :: xs => apply(xs,current,res:::List(x)) //inside camel phrase. New word that is not an acronym   
    }
    apply(ws,"",List())
  } 
  
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
	 }}
	 apply(methodName,"",List())
   }
  
  //Take a methodName as input and return a camel phrase with reconstructed acronyms
  //or an empty list if the method name is not a camel phrase
  def reconstructPhrase(methodName: String): List[String] = reconstructAcronym(splitWord(methodName))
  
  //For a specific word, return true if this word is an acronym
  def isAcronym(word: String):Boolean = {
    
    def checkTail(str: String): Boolean = str.head match {
      case x if(x.isLetter) => if(x.isUpperCase) {if(str.length>1) checkTail(str.tail) else true} 
      						   else false 
      case x if(x.isDigit) => if(str.length>1) checkTail(str.tail) else true
      case _ => false
    }
    
    if(word.isEmpty) false else word.head match {
      case x if(x.isLetter && word.length>1) => if(x.isUpperCase) checkTail(word.tail) else false
      case _ => false
    }
  }

}