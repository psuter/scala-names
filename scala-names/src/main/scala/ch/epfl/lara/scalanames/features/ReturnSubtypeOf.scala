package ch.epfl.lara.scalanames.features

trait ReturnSubtypeOf extends MethodFeature {
  import component._
  import component.global._
  
  val traitSymbol : Symbol
  
  lazy val name = "Method returns a subtype of "+traitSymbol.toString

  def appliesTo(methodDef: MethodDef): Boolean = {
    methodDef.rettype.baseType(traitSymbol) != NoType
  }

}