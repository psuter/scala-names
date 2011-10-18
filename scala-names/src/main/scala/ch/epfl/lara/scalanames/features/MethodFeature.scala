package ch.epfl.lara.scalanames.features

import ch.epfl.lara.scalanames._
import scala.tools.nsc.Global

trait MethodFeature {
  val component : AnalysisComponent
  import component._
  
  val name : String
  
  val id : Int
  
  def appliesTo(methodDef : MethodDef) : Boolean
}