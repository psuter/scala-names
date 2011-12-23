package ch.epfl.lara.scalanames.features

trait RightAssociative extends MethodFeature {
  import component._
  import component.global._

  val name = "This method is right associative"
    
  def appliesTo(methodDef: MethodDef): Boolean = methodDef.name.last==':'

}