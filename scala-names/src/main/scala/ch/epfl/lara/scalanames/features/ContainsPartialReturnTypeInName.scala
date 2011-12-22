package ch.epfl.lara.scalanames.features

trait ContainsPartialReturnTypeInName extends ContainsAcronym {
  import component._
  import component.global._
  
  override lazy val name = "Method return type is partially into the method name"
    
  override def appliesTo(methodDef: MethodDef): Boolean = { 
    
    //camel phrase
    val cp = reconstructAcronym(methodDef.name)
    if(cp.length>0){
      
      val upperCp = firstLetterToUpperCase(cp)
      //retrieve type  
      val typeName = resultType(methodDef.rettype)
      val splitedTypeName = reconstructAcronym(typeName)
	
	splitedTypeName.length match {
      case 0 => false
      case _ => oneInto(upperCp,splitedTypeName)
    }
    } else false
    
  }
  //As type always begin with upperCase letter, catch first letter of methodName, by convention a lowerCase, and transform it to upperCase
  def firstLetterToUpperCase(ws: List[String]):List[String]= 
    if(ws.length>0){
      if(ws.head.isEmpty()) firstLetterToUpperCase(ws.tail)
      else (ws.head.head.toUpperCase+ws.head.tail)::ws.tail 
    } else ws
  
  def oneInto(ws: List[String],ts: List[String]): Boolean = ws match {   
    case Nil => false
    case x :: Nil => ts.contains(==>(x))
    case x :: xs => ts.contains(==>(x)) || oneInto(xs,ts)
 }
  
  //Often used abreviation
  def ==> (word:String):String = word match {
    case "Str" | "str" => "String"
	case "Bool"| "bool"=> "Boolean"
	case "Int" | "int" => "Integer"
	case       x       => x
  }
  
  //From a Type return a sweet net typeName
  def resultType(t: Type):String = {
	//Unfold currified return type
	def unfoldMethodType(tt: Type):Type = tt match {
      case MethodType(ls,rt) => unfoldMethodType(rt)
      case NullaryMethodType(rt) => unfoldMethodType(rt)
	  case x => x
    }
    //remove java package dot	
	def removePackage(tn: String): String = {
	  if(tn.contains(".")) tn.split("\\u002E").last else tn
    }
    removePackage(((unfoldMethodType(t)).typeConstructor).toString)
  }
}