package ch.epfl.lara.scalanames.features

trait IsPublic extends MethodFeature {
  import component._
  import component.global._
  
  val name = "This method is public"

  def appliesTo(methodDef: MethodDef): Boolean = { 
    val symbol = methodDef.d.symbol
    
    symbol.isPublic
  }

}