package ch.epfl.lara.scalanames.features

trait IsValidJavaName extends MethodFeature {
  import component._
  import component.global._
  
  val name = "MethodName is a valid Java name."
    
  def appliesTo(methodDef: MethodDef): Boolean = { 
    
   //check tail of methodname. It's valid if it's composed only of letter, digit and underscore. 
   def checkTail(str: String): Boolean = str.head match {
      case x if(x.isLetterOrDigit || x.equals('_')) => if(str.length()>1) checkTail(str.tail) else true
      case _ => false
   }
   
    //Check first character. Java valid name start with a letter
    if(methodDef.name.head.isLetter){
      if(methodDef.name.length() >1) checkTail(methodDef.name)
      else true
    } else false
  }
  
  

}