package ch.epfl.lara.scalanames.features

trait ReturnTypeIs extends MethodFeature {
  import component._
  import component.global._
  
  val traitSymbol : Symbol
  
  lazy val name = "Method return is of type: "+traitSymbol.toString()+"."

  def appliesTo(methodDef: MethodDef): Boolean = { 
    traitSymbol.tpe =:= methodDef.rettype
  }
  
}