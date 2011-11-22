package ch.epfl.lara.scalanames.features

trait NameContains extends ContainsAcronym {
  import component._
  import component.global._
  
  val pattern : String

  override val name = "MethodName contains \""+pattern+"\" pattern."    
    
 override def appliesTo(methodDef: MethodDef): Boolean = {
    
    val UpperCasePattern = (pattern.head.toUpperCase)+pattern.tail

    val cp = reconstructAcronym(splitWord(methodDef.name,"",List()),"",List())
    
    def apply(ws: List[String]):Boolean = ws match {
      case Nil => false
      case x :: Nil => pattern==x||UpperCasePattern==x
      case x :: xs => if(pattern==x||UpperCasePattern==x) true else apply(xs)
    }
    apply(cp)
  }

}