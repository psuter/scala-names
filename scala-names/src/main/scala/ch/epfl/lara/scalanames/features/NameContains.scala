package ch.epfl.lara.scalanames.features

trait NameContains extends ContainsAcronym {
  import component._
  import component.global._
  
  val pattern : String

  override lazy val name = "MethodName contains \""+pattern+"\" pattern."    
    
 override def appliesTo(methodDef: MethodDef): Boolean = {
    
    //If this word is inside a camel phrase, it will begin with a UpperCase letter
    val UpperCasePattern = (pattern.head.toUpperCase)+pattern.tail

    //Reconstruct a phrase
    val cp = reconstructPhrase(methodDef.name)
    
    //test if it contains the given pattern
    def apply(ws: List[String]):Boolean = ws match {
      case Nil => false
      case x :: Nil => pattern==x||UpperCasePattern==x
      case x :: xs => if(pattern==x||UpperCasePattern==x) true else apply(xs)
    }
    apply(cp)
  }
}