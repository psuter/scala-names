package ch.epfl.lara.scalanames.features

trait ContainsAcronym extends IsCamelPhrase {
  import component._
  import component.global._
  
  override lazy val name = "Method name contains an acronym"
    
  /**
   * Acronym are defined as a list of capital letter or digit 
   */
    
  override def appliesTo(methodDef: MethodDef): Boolean = {
    val ws = splitWord(methodDef.name)
    if(ws.length > 1){
      val cp = reconstructAcronym(ws)
      cp.length < ws.length
    }
    else false //only 1 word, not an acronym
  }

  //Take the splited camel phrase as input and return a camel phrase with reconstructed acronyms
  def reconstructAcronym(ws: List[String]): List[String] = {
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
  
  //Take a methodName as input and return a camel phrase with reconstructed acronyms
  //or an empty list if the method name is not a camel phrase
  def reconstructAcronym(methodName: String): List[String] = reconstructAcronym(splitWord(methodName))
  
  //For a specific word, return true if this word is an acronym
  def isAcronym(word: String):Boolean = word.toUpperCase() == word 

}