package ch.epfl.lara.scalanames.features

import edu.smu.tspell.wordnet._

trait IsNoun extends IsVerb {
  import component._
  import component.global._
  
  override def feature(methodDef: MethodDef):Boolean = {
    database.getSynsets(methodDef.name, SynsetType.NOUN).length > 0
  }
}