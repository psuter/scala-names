package ch.epfl.lara.scalanames.features

trait NoParam extends MethodFeature {
  import component._
  import component.global._
  
  val name = "Method have not parameter."

  def appliesTo(methodDef: MethodDef): Boolean = methodDef.d.vparamss.size match {
      case 0 => true
      case 1 => methodDef.d.vparamss.head.isEmpty
      case _ => false
    }

  
}
