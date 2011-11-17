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
    if (name.isEmpty) res:::List(current)
    else {
      val head = name.head
      if (head.isLetter){
        if(head.isUpperCase) splitWord(name.tail,head.toString,res:::List(current))
        else splitWord(name.tail,current+head,res)  
      } else if (head.isDigit){
        splitWord(name.tail,head.toString(),res:::List(current))
      }
      else List()
    }
  }

}