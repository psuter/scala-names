package ch.epfl.lara.scalanames.features

import edu.smu.tspell.wordnet._

trait IsNoun extends MethodFeature {
  import component._
  import component.global._
  
  val name = "MethodName is a noun"
  val database : WordNetDatabase
  
  def appliesTo(methodDef: MethodDef):Boolean = {
    database.getSynsets(methodDef.name, SynsetType.NOUN).length > 0
  }
}