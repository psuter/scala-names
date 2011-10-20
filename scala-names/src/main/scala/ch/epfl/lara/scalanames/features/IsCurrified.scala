package ch.epfl.lara.scalanames.features


trait IsCurrified extends MethodFeature {
  import component._
  import component.global._

  val name = "Method is currified."
  
  def appliesTo(methodDef: MethodDef): Boolean = methodDef.d.vparamss.size match {
    case i if(i >1) => true
    case _ => false
  }

}