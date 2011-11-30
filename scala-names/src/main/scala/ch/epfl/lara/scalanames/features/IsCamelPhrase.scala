package ch.epfl.lara.scalanames.features


trait IsCamelPhrase extends MethodFeature {
  import component._
  import component.global._
  
  
  val name = "Method name is a camel phrase."

  def appliesTo(methodDef: MethodDef): Boolean = {
    val ws = splitWord(methodDef.name,"",List())
    //println("methodName: "+methodDef.name+" / splitWord: "+ws.toString)
    ws.length > 1
  }
  
  //Split camel phrase
  //return empty list if contains other things than letter or digit
  def splitWord(name: String, current: String, res: List[String]): List[String] = {
    
    def cur = if(current=="")res else res:::List(current)
    
    if (name.isEmpty) cur
    else {
      val head = name.head
      if (head.isLetter){ 												//a letter
        if(head.isUpperCase) splitWord(name.tail,head.toString,cur) 	//uppercase letter
        else splitWord(name.tail,current+head,res)  			    	//lowercase letter
      } else if (head.isDigit) splitWord(name.tail,head.toString,cur)	//a digit      
      else splitWord(name.tail,"",cur)									//a special character
      
    }
  }

}