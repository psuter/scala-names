package ch.epfl.lara.scalanames.features

trait IsOverride extends MethodFeature {
  import component._
  import component.global._
  
  val name = "Method is overriding another method"
    
  def appliesTo(methodDef: MethodDef): Boolean = { methodDef.d.symbol.isOverride }

}