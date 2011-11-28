package ch.epfl.lara.scalanames.features

trait IsOperator extends MethodFeature {
  import component._
  import component.global._
  
  override val name = "MethodName is an operator"

  def appliesTo(methodDef: MethodDef): Boolean = { 
    
    def traverseName(str: String):Boolean = str match {
      case "" => true
      case x if(x.head.isLetterOrDigit) => false
      case x => traverseName(x.tail)
    }
    
    traverseName(methodDef.name)
  }

}