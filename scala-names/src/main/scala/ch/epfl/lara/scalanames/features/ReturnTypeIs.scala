package ch.epfl.lara.scalanames.features

trait ReturnTypeIs extends MethodFeature {
  import component._
  import component.global._
  
  object SupportedType extends Enumeration {
    type SupportedType = Value

    val Int = Value("Int")
    val String = Value("String")
    val Boolean = Value("Boolean")
    val Unit = Value("Unit")
  }
  
  val ttype : SupportedType.SupportedType
  
  lazy val name = "Method return is of type: "+ttype+"."

  def appliesTo(methodDef: MethodDef): Boolean = ttype match { 
    case SupportedType.Int => getType("scala.Int") =:= methodDef.rettype
    case SupportedType.String => getType("java.lang.String") =:= methodDef.rettype
    case SupportedType.Boolean => getType("scala.Boolean") =:= methodDef.rettype
    case SupportedType.Unit => getType("scala.Unit") =:= methodDef.rettype
    case _ => false
  }
  
  private def getType(path: String): Type = global.definitions.getClass(path).tpe

}