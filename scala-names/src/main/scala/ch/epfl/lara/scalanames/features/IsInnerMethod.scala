package ch.epfl.lara.scalanames.features


trait IsInnerMethod extends MethodFeature {
  import component._
  import component.global._

  val name = "This method is defined inside another method"
  
  def appliesTo(methodDef: MethodDef): Boolean = { 
	methodDef.d.symbol.owner.isMethod
  }

}