package ch.epfl.lara.scalanames.features
import edu.smu.tspell.wordnet._


trait IsVerb extends MethodFeature {
  import component._
  import component.global._

    val name = "Method name is a verb"

      
  def appliesTo(methodDef: MethodDef): Boolean = { 
    System.setProperty("wordnet.database.dir", "C:\\Program Files\\WordNet\\2.1\\dict")
    
    
    val database = WordNetDatabase.getFileInstance
    val base : Array[String] = database.getBaseFormCandidates(methodDef.name,SynsetType.VERB)
    
    if(base.length > 0) true else false
    
  }

}
