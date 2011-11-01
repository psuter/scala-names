package ch.epfl.lara.scalanames.features

trait HasNoParentesis extends MethodFeature {
  import component._
  import component.global._
  
  val name = "Method has no parentesis."

  def appliesTo(methodDef: MethodDef): Boolean = methodDef.d.vparamss.size match {
    case 0 => true
    case _ => false
  }

}