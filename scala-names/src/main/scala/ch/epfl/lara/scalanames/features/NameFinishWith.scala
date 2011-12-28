package ch.epfl.lara.scalanames.features

trait NameFinishWith extends MethodFeature {
  import component._
  import component.global._
  
  val pattern : String
  
  lazy val name = "This method finish with "+pattern
    
  def appliesTo(methodDef: MethodDef): Boolean = pattern.length match {
    case 0 => false
    case 1 => methodDef.name.last == pattern.head
    case 2 if(methodDef.name.length > 1) => methodDef.name.takeRight(2) == pattern
    case x if(methodDef.name.length > (x-1)) => methodDef.name.takeRight(x) == pattern
    case _ => false
  }
}