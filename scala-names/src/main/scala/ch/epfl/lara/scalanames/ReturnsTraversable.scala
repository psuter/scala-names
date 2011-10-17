package ch.epfl.lara.scalanames

import scala.tools.nsc.Global

trait ReturnsTraversable { 
  self : Definitions =>
    
  import global._
  
  // Recovers the __symbol__ for the Traversable trait
  lazy val traversableTraitSymbol : Symbol = global.definitions.getClass("scala.collection.Traversable")
    
  def containsNamingBug(md : MethodDef) : Boolean = { 
    def isTraversable : Boolean = md.rettype.baseType(traversableTraitSymbol) != NoType
   
    def multipleElem : Boolean = isTraversable
      
    println("Return type : " + md.rettype)
    println(md.name + "/Traversable:" + isTraversable)
   
    println("")
     
    if(md.name.endsWith("s")) !multipleElem
    else multipleElem
  }
}