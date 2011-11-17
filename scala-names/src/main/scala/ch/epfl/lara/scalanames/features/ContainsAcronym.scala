package ch.epfl.lara.scalanames.features

trait ContainsAcronym extends IsCamelPhrase {
  import component._
  import component.global._
  
  override val name = "Method name contains an acronym"
    
  override def appliesTo(methodDef: MethodDef): Boolean = {
    val ws = splitWord(methodDef.name,"",List())
    if(ws.length > 1){
      val cp = reconstructAcronym(ws,"",List())
      cp.length < ws.length
    }
    else false //only 1 word, not an acronym
  }

  def reconstructAcronym(ws: List[String], current: String, res:List[String]): List[String] = ws match {
    case Nil => ws											//Empty list, should not happen
    case x :: Nil if(current=="")=> res:::List(x)			//At the end of a camel phrase. Previous word is not an acronym, this one either (or one letter)
    case x :: Nil if(x.length()==1)=> res:::List(current+x) //At the end of a camel phrase. Last word is an acronym
    case x :: Nil => res:::List(current,x)					//At the end of a camel phrase. Last word is not an acronym but previous is
    case x :: xs if(x.length()==1)=> reconstructAcronym(xs,current+x,res) //inside camel phrase. We discover a new acronym or continue one
    case x :: xs if(current != "" && x.length()!=1)=> reconstructAcronym(xs,"",res:::List(current,x)) //inside camel phrase. End of an acronym
    case x :: xs => reconstructAcronym(xs,current,res:::List(x)) //inside camel phrase. New word that is not an acronym   
  }
}