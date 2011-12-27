package ch.epfl.lara.scalanames.features

trait IsStatic extends MethodFeature {
  import component._
  import component.global._
  
  val name = "This method is static"
    
  def appliesTo(methodDef: MethodDef): Boolean = { methodDef.d.symbol.isStatic }

}