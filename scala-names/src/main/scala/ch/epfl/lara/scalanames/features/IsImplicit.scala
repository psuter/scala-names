package ch.epfl.lara.scalanames.features

trait IsImplicit extends MethodFeature {
  import component._
  import component.global._
  
  val name = "This method is implicit."
  def appliesTo(methodDef: MethodDef): Boolean = {methodDef.d.symbol.isImplicit }

}