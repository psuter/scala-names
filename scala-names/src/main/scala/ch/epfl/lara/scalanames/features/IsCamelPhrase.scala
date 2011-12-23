package ch.epfl.lara.scalanames.features


trait IsCamelPhrase extends ContainsAcronym {
  import component._
  import component.global._
  
  override lazy val name = "Method name is a camel phrase."

  /**A camel phrase is at least 2 words 
   * The first letter of each word except the first one should be a capital letter
   * camel phrase may contain acronym
   **/
  override def appliesTo(methodDef: MethodDef): Boolean = {
    val ws = reconstructPhrase(methodDef.name)  
    isCamelPhrase(ws)
  }
  
  def isCamelPhrase(words:List[String]) : Boolean = {
    
    def checkNextWord(ws:List[String]):Boolean = ws.head match {
      case x if(isAcronym(x)) => if(ws.length>1) checkNextWord(ws.tail) else true
      case x if(beginWithCapitalLetter(x)) => if(ws.length>1) checkNextWord(ws.tail) else true
      case _ => false
    }
    
    if(words.isEmpty) false else words.head.head match {
      case x if(x.isLetter) => if(words.length>1) checkNextWord(words.tail) else false
      case _ => false
    }
  }

  def beginWithCapitalLetter(word: String): Boolean = if(word.isEmpty) false else word.head match {
    case x if(x.isLetter) => x.isUpperCase
    case _ => false
  }
  
}