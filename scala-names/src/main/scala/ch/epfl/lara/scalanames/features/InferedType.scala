package ch.epfl.lara.scalanames.features

trait InferedType extends MethodFeature {
  import component._
  import component.global._
  
  val name = "The type of this method is inferred."

  def appliesTo(methodDef: MethodDef): Boolean = { 
    //println(methodDef.d.symbol.thisType)
    //println(methodDef.d.symbol.tpe)
    //println(methodDef.d.symbol.tpeHK)
    //println(methodDef.d.symbol.typeConstructor)
    //println(methodDef.d.symbol.typeOfThis)
    //println(methodDef.d.symbol.isAbstractType)
    //println(methodDef.d.isTyped)
    //println(methodDef.d.keyword)
    //println(methodDef.d.tpt)
    //println(methodDef.d.tparams)
    //println(methodDef.rettype)
    //println(methodDef.d.symbol.isExistentialQuantified)
    //println(methodDef.d.symbol.isImplOnly)
    /**NEED another phase to obtain this information**/
  false}

}