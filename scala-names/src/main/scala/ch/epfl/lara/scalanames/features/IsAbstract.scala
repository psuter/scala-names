package ch.epfl.lara.scalanames.features

trait IsAbstract extends MethodFeature {
  import component._
  import component.global._
  
  val name = "This Method is abstract"
    
  def appliesTo(methodDef: MethodDef): Boolean = { 
 /*   println("isAbstract:"+methodDef.d.symbol.isAbstract)
    println("isOAC:"+methodDef.d.symbol.owner.isAbstractClass)
    println("body:"+methodDef.d.rhs.isEmpty)*/
    
   methodDef.d.rhs.isEmpty}

}